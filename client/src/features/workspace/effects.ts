/**
 * Effects (Service Layer)
 * Orchestrates API calls, PubSub events, and State Actions.
 *
 * DESIGN: This layer handles the mapping between backend (name-based)
 * and frontend (ID-based) representations.
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
  BackendDecompositionResult,
  BackendWorkspaceResponse,
  Attribute,
  BackendHealth,
} from "./model";

import { type WorkspaceState, initialState } from "./state";
import * as Actions from "./actions";
import { API_BASE_URL } from "./config";

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
  | { _tag: "CROSS_TABLE_FD_ADDED"; fromId: TableId; toId: TableId }
  | { _tag: "CROSS_TABLE_FD_DELETED"; index: number }
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

// -- Errors --

export class ApiError extends Schema.TaggedError<ApiError>()("ApiError", {
  message: Schema.String,
}) {}

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
  readonly deleteFD: (relationId: TableId, index: number) => Effect.Effect<void>;
  readonly addCrossTableFD: (fromTableId: TableId, toTableId: TableId) => Effect.Effect<void>;
  readonly deleteCrossTableFD: (index: number) => Effect.Effect<void>;
  readonly normalizeRelation: (
    relationId: TableId,
    strategy: OptimizationStrategy
  ) => Effect.Effect<typeof BackendDecompositionResult.Type | undefined, ApiError>;
  readonly updateRelationPosition: (id: TableId, x: number, y: number) => Effect.Effect<void>;
  readonly optimizeWorkspace: (strategy: OptimizationStrategy) => Effect.Effect<void, ApiError>;
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

  const deleteFD = (relationId: TableId, index: number) =>
    updateAndPublish(Actions.deleteFD(relationId, index), {
      _tag: "RELATION_UPDATED",
      id: relationId,
    });

  const addCrossTableFD = (fromTableId: TableId, toTableId: TableId) =>
    updateAndPublish(Actions.addCrossTableFD(fromTableId, toTableId), {
      _tag: "CROSS_TABLE_FD_ADDED",
      fromId: fromTableId,
      toId: toTableId,
    });

  const deleteCrossTableFD = (index: number) =>
    updateAndPublish(Actions.deleteCrossTableFD(index), { _tag: "CROSS_TABLE_FD_DELETED", index });

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

  const toRelation = (
    br: typeof BackendRelation.Type,
    position: Position = { x: 0, y: 0 }
  ): Relation => ({
    id: crypto.randomUUID() as TableId,
    name: br.rjName,
    attributes: br.rjAttributes.map((s) => asAttribute(s)),
    fds: br.rjFDs.map((fd) => ({
      lhs: fd.fjLhs.map((s) => asAttribute(s)),
      rhs: fd.fjRhs.map((s) => asAttribute(s)),
    })),
    position,
  });

  const normalizeRelation = (
    relationId: TableId,
    strategy: OptimizationStrategy
  ): Effect.Effect<typeof BackendDecompositionResult.Type | undefined, ApiError> =>
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

      const response = yield* Effect.tryPromise({
        try: () =>
          fetch(`${API_BASE_URL}/api/normalize`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          }).then((res) => res.json()),
        catch: (e) => new ApiError({ message: String(e) }),
      });

      const result = yield* Schema.decodeUnknown(BackendDecompositionResult)(response).pipe(
        Effect.mapError((e) => new ApiError({ message: String(e) }))
      );

      // Create new relations with positions spread from anchor
      const newRelations = result.nresRelations.map((br, idx) =>
        toRelation(br, {
          x: anchor.x + (idx % 3) * 350,
          y: anchor.y + Math.floor(idx / 3) * 200,
        })
      );

      // Replace old relation with new ones
      yield* Ref.update(state, Actions.replaceRelation(relationId, newRelations));

      const newIds = newRelations.map((r) => r.id);
      yield* PubSub.publish(events, { _tag: "NORMALIZATION_COMPLETED", oldId: relationId, newIds });

      return result;
    });

  const optimizeWorkspace = (strategy: OptimizationStrategy): Effect.Effect<void, ApiError> =>
    Effect.gen(function* (_) {
      yield* PubSub.publish(events, { _tag: "ANALYSIS_STARTED" });

      const currentState = yield* Ref.get(state);
      const ws = currentState.present;

      // Convert HashMap to array for API
      const relationsArray = Array.from(HashMap.values(ws.relations));

      const response = yield* Effect.tryPromise({
        try: () =>
          fetch(`${API_BASE_URL}/api/workspace`, {
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
        catch: (e) => new ApiError({ message: String(e) }),
      });

      const result = yield* Schema.decodeUnknown(BackendWorkspaceResponse)(response).pipe(
        Effect.mapError((e) => new ApiError({ message: String(e) }))
      );

      if (result.wresSuccess) {
        // Build name→ID map for backend→frontend translation
        const nameToId = buildNameToIdMap(ws);

        // Map backend health (name-based) to frontend (ID-based)
        const newHealth = result.wresHealth
          .map((h) => {
            const tableId = nameToId.get(h.thjTableName);
            if (!tableId) return null;
            return {
              tableId,
              severity: (h.thjSeverity === "Critical"
                ? "error"
                : h.thjSeverity === "Warning"
                  ? "warning"
                  : "ok") as "ok" | "warning" | "error",
              message: h.thjMessage,
              suggestion: h.thjSuggestion,
            };
          })
          .filter((h): h is NonNullable<typeof h> => h !== null);

        // Map merge suggestions (name-based) to IDs
        const newMergeSuggestions: MergeSuggestion[] = result.wresMergeSuggestions
          .map(([n1, n2, reason]) => {
            const id1 = nameToId.get(n1);
            const id2 = nameToId.get(n2);
            if (!id1 || !id2) return null;
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

      const response = yield* Effect.tryPromise({
        try: () =>
          fetch(`${API_BASE_URL}/api/analyze`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          }).then((res) => res.json()),
        catch: (e) => new ApiError({ message: String(e) }),
      }).pipe(Effect.catchAll(() => Effect.succeed({} as any)));

      const result = yield* Schema.decodeUnknown(BackendAnalyzeResponse)(response).pipe(
        Effect.catchAll(() => Effect.succeed({ aresHealth: Option.none() } as any))
      );

      const healthOpt = result.aresHealth;
      if (Option.isSome(healthOpt)) {
        const h = healthOpt.value as any;
        const newHealth = {
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
            health: [...s.present.health.filter((lh) => lh.tableId !== relation.id), newHealth],
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
