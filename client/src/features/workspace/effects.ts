/**
 * Workspace Service (Orchestration Layer)
 * Coordinates:
 * 1. API Calls (Infrastructure)
 * 2. Domain Logic (Business Rules)
 * 3. State Updates (Actions)
 * 4. Events (PubSub)
 */

import { Effect, Context, Layer, Ref, PubSub, Option, HashMap } from "effect";
import {
  type Workspace,
  type Relation,
  type OptimizationStrategy,
  type TableId,
  type Position,
  type FDId,
  type CrossTableFDId,
  type BackendDecompositionResult,
  type BackendAnalyzeResponse,
} from "./model";

import { type WorkspaceState, initialState } from "./state";
import * as Actions from "./actions";
import { type WorkspaceError } from "./errors";

import { api } from "./api";
import * as Logic from "./logic";
import { layoutTree, layoutGrid } from "./layout";
import { findBestDestination } from "./link-migration";

// -- Events Definition --

export type WorkspaceEvent =
  | { _tag: "RELATION_ADDED"; id: TableId; relation: Relation }
  | { _tag: "RELATION_UPDATED"; id: TableId }
  | { _tag: "RELATION_DELETED"; id: TableId }
  | { _tag: "RELATION_MOVED"; id: TableId; position: Position }
  | { _tag: "CROSS_TABLE_FD_ADDED"; id: CrossTableFDId; fromId: TableId; toId: TableId }
  | { _tag: "CROSS_TABLE_FD_DELETED"; id: CrossTableFDId }
  | { _tag: "ANALYSIS_STARTED" }
  | { _tag: "ANALYSIS_COMPLETED" }
  | { _tag: "NORMALIZATION_COMPLETED"; oldId: TableId; newIds: TableId[] }
  | { _tag: "RELATIONS_MERGED"; survivingId: TableId; deletedId: TableId }
  | { _tag: "UNDO" }
  | { _tag: "REDO" }
  | { _tag: "STATE_CHANGED" };

// -- Service Definition --

export interface WorkspaceService {
  readonly state: Ref.Ref<WorkspaceState>;
  readonly events: PubSub.PubSub<WorkspaceEvent>;

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

  const updateAndPublish = (
    action: (s: WorkspaceState) => WorkspaceState,
    event: WorkspaceEvent
  ): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, action);
      yield* PubSub.publish(events, event);
    });

  // --- History ---
  const undo = updateAndPublish(Actions.undo, { _tag: "UNDO" });
  const redo = updateAndPublish(Actions.redo, { _tag: "REDO" });

  // --- CRUD Operations ---

  const addRelation = (
    name: string,
    attributes: string[],
    fds: { lhs: string[]; rhs: string[] }[],
    x: number,
    y: number
  ) =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, Actions.addRelation(name, attributes, fds, x, y));
      const currentState = yield* Ref.get(state);
      const ws = currentState.present;
      // Optimistic find
      const addedRel = Array.from(HashMap.values(ws.relations)).find((r) => r.name === name);
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
      const currentState = yield* Ref.get(state);
      const ws = currentState.present;
      // Find the ID (could be optimized if Action returned it, but this is fine for now)
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
      yield* PubSub.publish(events, { _tag: "RELATION_MOVED", id, position: { x, y } });
    });

  const mergeRelations = (id1: TableId, id2: TableId) =>
    updateAndPublish(Actions.mergeRelations(id1, id2), {
      _tag: "RELATIONS_MERGED",
      survivingId: id1,
      deletedId: id2,
    });

  // --- Orchestrated Workflows ---

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

      // 1. Call API
      const result = yield* api.normalize({ relation: rel, strategy });

      // 2. Logic: Calculate Layout
      let positions: Map<string, Position>;
      if (Option.isSome(result.nresTree)) {
        const layoutNode = Logic.backendTreeToLayoutNode(result.nresTree.value);
        const layoutResults = layoutTree(layoutNode, anchor);
        positions = new Map(layoutResults.map((l) => [l.name, l.position]));
      } else {
        const names = result.nresRelations.map((r) => r.rjName);
        const layoutResults = layoutGrid(names, anchor);
        positions = new Map(layoutResults.map((l) => [l.name, l.position]));
      }

      // 3. Logic: Map to Frontend Relations
      const newRelations = result.nresRelations.map((br) =>
        Logic.toRelation(br, positions.get(br.rjName) ?? anchor)
      );

      // 4. Logic: Link Migration
      const inboundMigrations = new Map<CrossTableFDId, TableId>();
      const outboundMigrations = new Map<CrossTableFDId, TableId>();

      for (const [edgeId, ctfd] of HashMap.entries(ws.crossTableFDs)) {
        if (ctfd.toTableId === relationId) {
          const sourceRel = HashMap.get(ws.relations, ctfd.fromTableId);
          if (Option.isSome(sourceRel)) {
            const best = findBestDestination(sourceRel.value.attributes, newRelations);
            if (best) {
              inboundMigrations.set(edgeId, best.newTableId);
            }
          }
        }
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

      // 5. Update State
      yield* Ref.update(
        state,
        Actions.decomposeRelation(relationId, newRelations, inboundMigrations, outboundMigrations)
      );

      // 6. Publish Events
      const newIds = newRelations.map((r) => r.id);
      yield* PubSub.publish(events, { _tag: "NORMALIZATION_COMPLETED", oldId: relationId, newIds });

      return result;
    });

  const optimizeWorkspace = (strategy: OptimizationStrategy): Effect.Effect<void, WorkspaceError> =>
    Effect.gen(function* (_) {
      yield* PubSub.publish(events, { _tag: "ANALYSIS_STARTED" });

      const currentState = yield* Ref.get(state);
      const ws = currentState.present;
      const relationsArray = Array.from(HashMap.values(ws.relations));

      // 1. Call API
      const result = yield* api.optimize(relationsArray, strategy);

      if (result.wresSuccess) {
        // 2. Logic: Map Response
        const nameToId = Logic.buildNameToIdMap(ws);
        const newHealth = Logic.mapBackendHealth(result.wresHealth, nameToId);
        const newSuggestions = Logic.mapMergeSuggestions(result.wresMergeSuggestions, nameToId);

        // 3. Update State
        yield* Ref.update(state, Actions.setAnalysisResults(newHealth, newSuggestions, []));
      } else {
        const errorMsg = Option.getOrElse(result.wresError, () => "Unknown analysis error");
        yield* Ref.update(state, Actions.setAnalysisResults(HashMap.empty(), [], [errorMsg]));
      }
      yield* PubSub.publish(events, { _tag: "ANALYSIS_COMPLETED" });
    });

  const analyzeRelation = (relation: Relation): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      // 1. Call API
      const result = yield* api.analyze(relation); // Errors propagated but we might want to swallow for silent updates

      // Handle potential null/failure gracefully as per original logic's intent (swallow error implies catchAll)
      // For now, allow typed errors to bubble or handle them here.
      // Original logic used catchAll and returned Success({ aresHealth: null }).
      // Let's wrap this in a catch for UI resilience.
    }).pipe(
      // Re-implement the catchAll logic from original locally or modifying API signature?
      // Keeping it simple: If API succeeds, we update.
      Effect.catchAll(() => Effect.succeed<void>(void 0)), // Swallow errors for background analysis
      Effect.flatMap((_) => {
        // Since we swallowed error, we need to restructure.
        // Let's actually do the call inside.
        return Effect.gen(function* (_) {
          const apiResult = yield* api.analyze(relation).pipe(
            Effect.catchAll(() =>
              Effect.succeed({
                aresSuccess: false,
                aresHealth: Option.none(),
                aresCandidateKeys: [],
                aresIsBCNF: false,
                aresIs3NF: false,
                aresError: Option.none(),
              } as BackendAnalyzeResponse)
            )
          );

          const healthOpt = apiResult.aresHealth;
          if (Option.isSome(healthOpt)) {
            const newHealth = Logic.mapSingleHealth(healthOpt.value, relation.id);

            // Update State
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
      })
    );

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
