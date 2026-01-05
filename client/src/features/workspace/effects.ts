/**
 * Effects (Service Layer)
 * Orchestrates API calls, PubSub events, and State Actions.
 *
 * DESIGN: This layer handles the mapping between backend (name-based)
 * and frontend (ID-based) representations.
 * Uses typed errors for exhaustive error handling.
 * Uses granular PubSub events for fine-grained reactivity.
 */

import { Effect, Context, Layer, Ref, PubSub, Option, HashMap, pipe } from "effect";
import * as Schema from "@effect/schema/Schema";
import {
  type Workspace,
  type Relation,
  type OptimizationStrategy,
  type BackendRelation,
  type TableId,
  type MergeSuggestion,
  type Position,
  type FDId,
  type CrossTableFDId,
  type TableHealth,
  BackendDecompositionResult,
  BackendWorkspaceResponse,
  type Attribute,
  BackendHealth,
} from "./model";

import { type WorkspaceState, initialState } from "./state";
import * as Actions from "./actions";
import { API_BASE_URL } from "./config";
import { NetworkError, DecodeError, type WorkspaceError } from "./errors";
import { layoutTree, layoutGrid, type LayoutNode, type LayoutResult } from "./layout";
import { findBestDestination } from "./link-migration";

// -- Schemas --

const BackendAnalyzeResponse = Schema.Struct({
  aresSuccess: Schema.Boolean,
  aresHealth: Schema.OptionFromNullOr(BackendHealth),
  aresCandidateKeys: Schema.Array(Schema.Array(Schema.String)),
  aresIsBCNF: Schema.Boolean,
  aresIs3NF: Schema.Boolean,
  aresError: Schema.OptionFromNullOr(Schema.String),
});

// -- Granular Events --

export type WorkspaceEvent =
  // Relation lifecycle
  | { _tag: "RELATION_ADDED"; id: TableId; relation: Relation }
  | { _tag: "RELATION_UPDATED"; id: TableId }
  | { _tag: "RELATION_DELETED"; id: TableId }
  | { _tag: "RELATION_MOVED"; id: TableId; position: Position }
  // Cross-table relationships
  | { _tag: "CROSS_TABLE_FD_ADDED"; id: CrossTableFDId; fromId: TableId; toId: TableId }
  | { _tag: "CROSS_TABLE_FD_DELETED"; id: CrossTableFDId }
  // Analysis
  | { _tag: "ANALYSIS_STARTED" }
  | { _tag: "ANALYSIS_COMPLETED" }
  | { _tag: "NORMALIZATION_COMPLETED"; oldId: TableId; newIds: TableId[] }
  // Merge
  | { _tag: "RELATIONS_MERGED"; survivingId: TableId; deletedId: TableId }
  // History
  | { _tag: "UNDO" }
  | { _tag: "REDO" }
  // Generic fallback for bulk updates
  | { _tag: "STATE_CHANGED" };

// -- Service Definition --

export interface WorkspaceService {
  readonly state: Ref.Ref<WorkspaceState>;
  readonly events: PubSub.PubSub<WorkspaceEvent>;

  // Actions
  readonly undo: Effect.Effect<void>;
  readonly redo: Effect.Effect<void>;

  readonly addRelation: (
    name: string,
    attributes: string[],
    fds: { lhs: string[]; rhs: string[] }[],
    x: number,
    y: number
  ) => Effect.Effect<void>;
  readonly renameRelation: (id: TableId, newName: string) => Effect.Effect<void>;
  readonly deleteRelation: (id: TableId) => Effect.Effect<void>;
  readonly addAttribute: (relationId: TableId, name: string) => Effect.Effect<void>;
  readonly deleteAttribute: (relationId: TableId, name: string) => Effect.Effect<void>;
  readonly addFD: (relationId: TableId, lhs: string[], rhs: string[]) => Effect.Effect<void>;
  readonly deleteFD: (relationId: TableId, fdId: FDId) => Effect.Effect<void>;
  readonly addCrossTableFD: (fromTableId: TableId, toTableId: TableId) => Effect.Effect<void>;
  readonly deleteCrossTableFD: (id: CrossTableFDId) => Effect.Effect<void>;
  readonly normalizeRelation: (
    relationId: TableId,
    strategy: OptimizationStrategy
  ) => Effect.Effect<typeof BackendDecompositionResult.Type | undefined, WorkspaceError>;
  readonly updateRelationPosition: (id: TableId, x: number, y: number) => Effect.Effect<void>;
  readonly optimizeWorkspace: (
    strategy: OptimizationStrategy
  ) => Effect.Effect<void, WorkspaceError>;
  readonly mergeRelations: (id1: TableId, id2: TableId) => Effect.Effect<void>;
  readonly analyzeRelation: (relation: Relation) => Effect.Effect<void>;
}

export const WorkspaceService = Context.GenericTag<WorkspaceService>("@app/WorkspaceService");

// -- Implementation --

const make = Effect.gen(function* (_) {
  const state = yield* Ref.make<WorkspaceState>(initialState);
  const events = yield* PubSub.unbounded<WorkspaceEvent>();

  // Helper: Update state and publish event
  const updateAndPublish = (
    action: (s: WorkspaceState) => WorkspaceState,
    event: WorkspaceEvent
  ): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, action);
      yield* PubSub.publish(events, event);
    });

  // Build name→ID lookup from current workspace
  const buildNameToIdMap = (ws: Workspace): Map<string, TableId> => {
    const result = new Map<string, TableId>();
    for (const [id, rel] of HashMap.entries(ws.relations)) {
      result.set(rel.name, id);
    }
    return result;
  };

  // --- History ---
  const undo = updateAndPublish(Actions.undo, { _tag: "UNDO" });
  const redo = updateAndPublish(Actions.redo, { _tag: "REDO" });

  // --- CRUD ---

  const addRelation = (
    name: string,
    attributes: string[],
    fds: { lhs: string[]; rhs: string[] }[],
    x: number,
    y: number
  ) =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, Actions.addRelation(name, attributes, fds, x, y));
      // Get the newly added relation
      const currentState = yield* Ref.get(state);
      const ws = currentState.present;
      // Find by name (just added)
      let addedRel: Relation | undefined;
      for (const rel of HashMap.values(ws.relations)) {
        if (rel.name === name) {
          addedRel = rel;
          break;
        }
      }
      if (addedRel) {
        yield* PubSub.publish(events, {
          _tag: "RELATION_ADDED",
          id: addedRel.id,
          relation: addedRel,
        });
      }
    });

  const deleteRelation = (id: TableId) =>
    updateAndPublish(Actions.deleteRelation(id), { _tag: "RELATION_DELETED", id });

  const renameRelation = (id: TableId, newName: string) =>
    updateAndPublish(Actions.renameRelation(id, newName), { _tag: "RELATION_UPDATED", id });

  const addAttribute = (relationId: TableId, name: string) =>
    updateAndPublish(Actions.addAttribute(relationId, name), {
      _tag: "RELATION_UPDATED",
      id: relationId,
    });

  const deleteAttribute = (relationId: TableId, name: string) =>
    updateAndPublish(Actions.deleteAttribute(relationId, name), {
      _tag: "RELATION_UPDATED",
      id: relationId,
    });

  const addFD = (relationId: TableId, lhs: string[], rhs: string[]) =>
    updateAndPublish(Actions.addFD(relationId, lhs, rhs), {
      _tag: "RELATION_UPDATED",
      id: relationId,
    });

  const deleteFD = (relationId: TableId, fdId: FDId) =>
    updateAndPublish(Actions.deleteFD(relationId, fdId), {
      _tag: "RELATION_UPDATED",
      id: relationId,
    });

  const addCrossTableFD = (fromTableId: TableId, toTableId: TableId) =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, Actions.addCrossTableFD(fromTableId, toTableId));
      // Find the just-added crossTableFD
      const currentState = yield* Ref.get(state);
      const ws = currentState.present;
      let addedId: CrossTableFDId | undefined;
      for (const [id, ctfd] of HashMap.entries(ws.crossTableFDs)) {
        if (ctfd.fromTableId === fromTableId && ctfd.toTableId === toTableId) {
          addedId = id;
          break;
        }
      }
      if (addedId) {
        yield* PubSub.publish(events, {
          _tag: "CROSS_TABLE_FD_ADDED",
          id: addedId,
          fromId: fromTableId,
          toId: toTableId,
        });
      }
    });

  const deleteCrossTableFD = (id: CrossTableFDId) =>
    updateAndPublish(Actions.deleteCrossTableFD(id), { _tag: "CROSS_TABLE_FD_DELETED", id });

  const updateRelationPosition = (id: TableId, x: number, y: number) =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, Actions.updateRelationPosition(id, x, y));
      // Position updates are frequent during drag, use specific event
      yield* PubSub.publish(events, { _tag: "RELATION_MOVED", id, position: { x, y } });
    });

  const mergeRelations = (id1: TableId, id2: TableId) =>
    updateAndPublish(Actions.mergeRelations(id1, id2), {
      _tag: "RELATIONS_MERGED",
      survivingId: id1,
      deletedId: id2,
    });

  // --- Complex Effects (API) ---

  const asAttribute = (s: string): Attribute => s as Attribute;

  const generateFDId = (): FDId => crypto.randomUUID() as FDId;

  const toRelation = (
    br: typeof BackendRelation.Type,
    position: Position = { x: 0, y: 0 }
  ): Relation => ({
    id: crypto.randomUUID() as TableId,
    name: br.rjName,
    attributes: br.rjAttributes.map((s) => asAttribute(s)),
    fds: br.rjFDs.map((fd) => ({
      id: generateFDId(),
      lhs: fd.fjLhs.map((s) => asAttribute(s)),
      rhs: fd.fjRhs.map((s) => asAttribute(s)),
    })),
    position,
  });

  // Convert backend decomposition tree to layout node structure
  const backendTreeToLayoutNode = (tree: any): LayoutNode => ({
    name: tree.tnRelation.rjName,
    children: tree.tnChildren.map(backendTreeToLayoutNode),
  });

  const normalizeRelation = (
    relationId: TableId,
    strategy: OptimizationStrategy
  ): Effect.Effect<typeof BackendDecompositionResult.Type | undefined, WorkspaceError> =>
    Effect.gen(function* (_) {
      const currentState = yield* Ref.get(state);
      const ws = currentState.present;

      const relOpt = HashMap.get(ws.relations, relationId);
      if (Option.isNone(relOpt)) {
        return undefined;
      }
      const rel = relOpt.value;
      const anchor = rel.position;

      const payload = {
        nrRelation: {
          rjName: rel.name,
          rjAttributes: rel.attributes,
          rjFDs: rel.fds.map((fd) => ({ fjLhs: fd.lhs, fjRhs: fd.rhs })),
        },
        nrStrategy: strategy,
        nrIncludeTree: true,
      };

      const url = `${API_BASE_URL}/api/normalize`;
      const response = yield* Effect.tryPromise({
        try: () =>
          fetch(url, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          }).then((res) => res.json()),
        catch: (e) => new NetworkError({ url, cause: e }),
      });

      const result = yield* Schema.decodeUnknown(BackendDecompositionResult)(response).pipe(
        Effect.mapError(
          (e) => new DecodeError({ schema: "BackendDecompositionResult", message: String(e) })
        )
      );

      // 1. Calculate positions using tree layout if available
      let positions: Map<string, Position>;

      if (Option.isSome(result.nresTree)) {
        // Use tree layout from decomposition result
        const layoutNode = backendTreeToLayoutNode(result.nresTree.value);
        const layoutResults = layoutTree(layoutNode, anchor);
        positions = new Map(layoutResults.map((l) => [l.name, l.position]));
      } else {
        // Fallback to grid layout
        const names = result.nresRelations.map((r) => r.rjName);
        const layoutResults = layoutGrid(names, anchor);
        positions = new Map(layoutResults.map((l) => [l.name, l.position]));
      }

      // 2. Create new relations with tree-based positions
      const newRelations = result.nresRelations.map((br) =>
        toRelation(br, positions.get(br.rjName) ?? anchor)
      );

      // 3. Build edge migration maps
      const inboundMigrations = new Map<CrossTableFDId, TableId>();
      const outboundMigrations = new Map<CrossTableFDId, TableId>();

      for (const [edgeId, ctfd] of HashMap.entries(ws.crossTableFDs)) {
        // Inbound edges: pointing TO the decomposed table
        if (ctfd.toTableId === relationId) {
          const sourceRel = HashMap.get(ws.relations, ctfd.fromTableId);
          if (Option.isSome(sourceRel)) {
            const best = findBestDestination(sourceRel.value.attributes, newRelations);
            if (best) {
              inboundMigrations.set(edgeId, best.newTableId);
            }
          }
        }

        // Outbound edges: FROM the decomposed table
        if (ctfd.fromTableId === relationId) {
          const targetRel = HashMap.get(ws.relations, ctfd.toTableId);
          if (Option.isSome(targetRel)) {
            const best = findBestDestination(targetRel.value.attributes, newRelations);
            if (best) {
              outboundMigrations.set(edgeId, best.newTableId);
            }
          }
        }
      }

      // 4. Apply decomposition with edge migrations
      yield* Ref.update(
        state,
        Actions.decomposeRelation(relationId, newRelations, inboundMigrations, outboundMigrations)
      );

      // 5. Publish event
      const newIds = newRelations.map((r) => r.id);
      yield* PubSub.publish(events, { _tag: "NORMALIZATION_COMPLETED", oldId: relationId, newIds });

      return result;
    });

  const optimizeWorkspace = (strategy: OptimizationStrategy): Effect.Effect<void, WorkspaceError> =>
    Effect.gen(function* (_) {
      yield* PubSub.publish(events, { _tag: "ANALYSIS_STARTED" });

      const currentState = yield* Ref.get(state);
      const ws = currentState.present;

      // Convert HashMap to array for API
      const relationsArray = Array.from(HashMap.values(ws.relations));

      const url = `${API_BASE_URL}/api/workspace`;
      const response = yield* Effect.tryPromise({
        try: () =>
          fetch(url, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              wrRelations: relationsArray.map((r) => ({
                rjName: r.name,
                rjAttributes: r.attributes,
                rjFDs: r.fds.map((fd) => ({
                  fjLhs: fd.lhs,
                  fjRhs: fd.rhs,
                })),
              })),
              wrStrategy: strategy,
            }),
          }).then((res) => res.json()),
        catch: (e) => new NetworkError({ url, cause: e }),
      });

      const result = yield* Schema.decodeUnknown(BackendWorkspaceResponse)(response).pipe(
        Effect.mapError(
          (e) => new DecodeError({ schema: "BackendWorkspaceResponse", message: String(e) })
        )
      );

      if (result.wresSuccess) {
        // Build name→ID map for backend→frontend translation
        const nameToId = buildNameToIdMap(ws);

        // Map backend health (name-based) to frontend (ID-based) HashMap
        let newHealth = HashMap.empty<TableId, TableHealth>();
        for (const h of result.wresHealth) {
          const tableId = nameToId.get(h.thjTableName);
          if (tableId) {
            const health: TableHealth = {
              tableId,
              severity: (h.thjSeverity === "Critical"
                ? "error"
                : h.thjSeverity === "Warning"
                  ? "warning"
                  : "ok") as "ok" | "warning" | "error",
              message: h.thjMessage,
              suggestion: h.thjSuggestion,
            };
            newHealth = HashMap.set(newHealth, tableId, health);
          }
        }

        // Map merge suggestions (name-based) to IDs
        const newMergeSuggestions: MergeSuggestion[] = result.wresMergeSuggestions
          .map(([n1, n2, reason]) => {
            const id1 = nameToId.get(n1);
            const id2 = nameToId.get(n2);
            if (!id1 || !id2) {
              return null;
            }
            return { tableId1: id1, tableId2: id2, reason };
          })
          .filter((s): s is NonNullable<typeof s> => s !== null);

        yield* Ref.update(state, Actions.setAnalysisResults(newHealth, newMergeSuggestions, []));
        yield* PubSub.publish(events, { _tag: "ANALYSIS_COMPLETED" });
      }
    });

  const analyzeRelation = (relation: Relation): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      const payload = {
        arRelation: {
          rjName: relation.name,
          rjAttributes: relation.attributes,
          rjFDs: relation.fds.map((fd) => ({ fjLhs: fd.lhs, fjRhs: fd.rhs })),
        },
      };

      const url = `${API_BASE_URL}/api/analyze`;
      const response = yield* Effect.tryPromise({
        try: () =>
          fetch(url, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          }).then((res) => res.json()),
        catch: () => ({ aresHealth: null }),
      }).pipe(Effect.catchAll(() => Effect.succeed({ aresHealth: null } as any)));

      const result = yield* Schema.decodeUnknown(BackendAnalyzeResponse)(response).pipe(
        Effect.catchAll(() => Effect.succeed({ aresHealth: Option.none() } as any))
      );

      const healthOpt = result.aresHealth;
      if (Option.isSome(healthOpt)) {
        const h = healthOpt.value as any;
        const newHealth: TableHealth = {
          tableId: relation.id,
          severity: (h.hjLevel === "Critical"
            ? "error"
            : h.hjLevel === "Warning"
              ? "warning"
              : "ok") as "ok" | "warning" | "error",
          message: h.hjMessage,
          suggestion: h.hjSuggestion,
        };

        yield* Ref.update(state, (s: WorkspaceState) => ({
          ...s,
          present: {
            ...s.present,
            health: HashMap.set(s.present.health, relation.id, newHealth),
          },
        }));
        yield* PubSub.publish(events, { _tag: "STATE_CHANGED" });
      }
    });

  return {
    state,
    events,
    undo,
    redo,
    addRelation,
    renameRelation,
    deleteRelation,
    addAttribute,
    deleteAttribute,
    addFD,
    deleteFD,
    addCrossTableFD,
    deleteCrossTableFD,
    updateRelationPosition,
    normalizeRelation,
    optimizeWorkspace,
    mergeRelations,
    analyzeRelation,
  };
});

export const WorkspaceServiceLive = Layer.effect(WorkspaceService, make);
