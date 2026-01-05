/**
 * Effects (Service Layer)
 * Orchestrates API calls, PubSub events, and State Actions.
 */

import { Effect, Context, Layer, Ref, PubSub, Option } from "effect";
import * as Schema from "@effect/schema/Schema";
import {
  type Workspace,
  type Relation,
  type OptimizationStrategy,
  type BackendRelation,
  BackendDecompositionResult,
  BackendWorkspaceResponse,
  Attribute,
  type TableId,
  BackendHealth,
} from "./model";

import { type WorkspaceState, initialState } from "./state";
import * as Actions from "./actions";

// -- Schemas --

const BackendAnalyzeResponse = Schema.Struct({
  aresSuccess: Schema.Boolean,
  aresHealth: Schema.OptionFromNullOr(BackendHealth),
  aresCandidateKeys: Schema.Array(Schema.Array(Schema.String)),
  aresIsBCNF: Schema.Boolean,
  aresIs3NF: Schema.Boolean,
  aresError: Schema.OptionFromNullOr(Schema.String),
});

// -- Events --

export type WorkspaceEvent =
  | { _tag: "STATE_UPDATED" }
  | { _tag: "DECOMPOSITION_RESULT_RECEIVED"; result: typeof BackendDecompositionResult.Type }
  | { _tag: "RELATION_ADDED"; relation: Relation }
  | { _tag: "RELATION_UPDATED"; relation: Relation }
  | { _tag: "RELATION_DELETED"; id: string }
  | { _tag: "NORMALIZATION_COMPLETED" }
  | { _tag: "WORKSPACE_OPTIMIZED" };

// -- Errors --

export class ApiError extends Schema.TaggedError<ApiError>()("ApiError", {
  message: Schema.String,
}) { }

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
  readonly renameRelation: (id: string, newName: string) => Effect.Effect<void>;
  readonly deleteRelation: (id: string) => Effect.Effect<void>;
  readonly addAttribute: (relationId: string, name: string) => Effect.Effect<void>;
  readonly deleteAttribute: (relationId: string, name: string) => Effect.Effect<void>;
  readonly addFD: (relationId: string, lhs: string[], rhs: string[]) => Effect.Effect<void>;
  readonly deleteFD: (relationId: string, index: number) => Effect.Effect<void>;
  readonly addCrossTableFD: (fromTableId: string, toTableId: string) => Effect.Effect<void>;
  readonly deleteCrossTableFD: (index: number) => Effect.Effect<void>;
  readonly normalizeRelation: (
    relationId: string,
    strategy: OptimizationStrategy
  ) => Effect.Effect<typeof BackendDecompositionResult.Type | undefined, ApiError>;
  readonly updateRelationPosition: (id: string, x: number, y: number) => Effect.Effect<void>;
  readonly optimizeWorkspace: (strategy: OptimizationStrategy) => Effect.Effect<void, ApiError>;
  readonly mergeRelations: (name1: string, name2: string) => Effect.Effect<void>;
  readonly analyzeRelation: (relation: Relation) => Effect.Effect<void>;
}

export const WorkspaceService = Context.GenericTag<WorkspaceService>("@app/WorkspaceService");

// -- Implementation --

const make = Effect.gen(function* (_) {
  const state = yield* Ref.make<WorkspaceState>(initialState);
  const events = yield* PubSub.unbounded<WorkspaceEvent>();

  // Helpers
  const updateAndNotify = (f: (s: WorkspaceState) => WorkspaceState): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, f);
      yield* PubSub.publish(events, { _tag: "STATE_UPDATED" });
    });

  // --- History ---
  const undo = updateAndNotify(Actions.undo);
  const redo = updateAndNotify(Actions.redo);

  // --- CRUD ---

  const addRelation = (
    name: string,
    attributes: string[],
    fds: { lhs: string[]; rhs: string[] }[],
    x: number,
    y: number
  ) => updateAndNotify(Actions.addRelation(name, attributes, fds, x, y));

  const deleteRelation = (id: string) =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, Actions.deleteRelation(id));
      yield* PubSub.publish(events, { _tag: "STATE_UPDATED" });
    });

  const renameRelation = (id: string, newName: string) => updateAndNotify(Actions.renameRelation(id, newName));

  const addAttribute = (relationId: string, name: string) => updateAndNotify(Actions.addAttribute(relationId, name));

  const deleteAttribute = (relationId: string, name: string) => updateAndNotify(Actions.deleteAttribute(relationId, name));

  const addFD = (relationId: string, lhs: string[], rhs: string[]) => updateAndNotify(Actions.addFD(relationId, lhs, rhs));

  const deleteFD = (relationId: string, index: number) => updateAndNotify(Actions.deleteFD(relationId, index));

  const addCrossTableFD = (fromTableId: string, toTableId: string) => updateAndNotify(Actions.addCrossTableFD(fromTableId, toTableId));

  const deleteCrossTableFD = (index: number) => updateAndNotify(Actions.deleteCrossTableFD(index));

  const updateRelationPosition = (id: string, x: number, y: number) => Ref.update(state, Actions.updateRelationPosition(id, x, y)); // No notification needed for drag

  const mergeRelations = (name1: string, name2: string) => updateAndNotify(Actions.mergeRelations(name1, name2));

  // --- Complex Effects (API) ---

  const asAttribute = (s: string): Attribute => s as Attribute;

  const toRelation = (br: typeof BackendRelation.Type): Relation => ({
    id: br.rjName as TableId,
    name: br.rjName,
    attributes: br.rjAttributes.map((s) => asAttribute(s)),
    fds: br.rjFDs.map((fd) => ({
      lhs: fd.fjLhs.map((s) => asAttribute(s)),
      rhs: fd.fjRhs.map((s) => asAttribute(s)),
    })),
    position: { x: 0, y: 0 },
  });

  const normalizeRelation = (
    relationId: string,
    strategy: OptimizationStrategy
  ): Effect.Effect<typeof BackendDecompositionResult.Type | undefined, ApiError> =>
    Effect.gen(function* (_) {
      const currentState = yield* Ref.get(state);
      const ws = currentState.present;

      const rel = ws.relations.find((r) => r.id === relationId);

      if (!rel) {
        return undefined;
      }

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
          fetch("http://localhost:8080/api/normalize", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          }).then((res) => res.json()),
        catch: (e) => new ApiError({ message: String(e) }),
      });

      const result = yield* Schema.decodeUnknown(BackendDecompositionResult)(response).pipe(
        Effect.mapError((e) => new ApiError({ message: String(e) }))
      );

      // Atomic replacement of relation
      yield* Ref.update(state, (s) => Actions.push(s, {
        ...s.present,
        relations: [
          ...s.present.relations.filter(r => r.id !== relationId),
          ...result.nresRelations.map(toRelation)
        ]
      }));

      yield* PubSub.publish(events, { _tag: "STATE_UPDATED" });

      return result;
    });

  const optimizeWorkspace = (strategy: OptimizationStrategy): Effect.Effect<void, ApiError> =>
    Effect.gen(function* (_) {
      const currentState = yield* Ref.get(state);
      const ws = currentState.present;

      const response = yield* Effect.tryPromise({
        try: () =>
          fetch("http://localhost:8080/api/workspace", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              wrRelations: ws.relations.map((r) => ({
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
        const newRelations = result.wresResults.flatMap(([_, rels]) => rels.map(toRelation));

        const newHealth = result.wresHealth.map((h) => ({
          tableName: h.thjTableName,
          severity: (h.thjSeverity === "Critical" ? "error" : h.thjSeverity === "Warning" ? "warning" : "ok") as "ok" | "warning" | "error",
          message: h.thjMessage,
          suggestion: h.thjSuggestion,
        }));

        // Update state with results
        yield* Ref.update(state, Actions.setAnalysisResults(newRelations, newHealth, result.wresMergeSuggestions, []));

        yield* PubSub.publish(events, { _tag: "STATE_UPDATED" });
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

      // We explicitly ignore errors here so that the UI doesn't break if the background check fails
      const response = yield* Effect.tryPromise({
        try: () =>
          fetch("http://localhost:8080/api/analyze", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          }).then((res) => res.json()),
        catch: (e) => new ApiError({ message: String(e) }),
      }).pipe(Effect.catchAll(() => Effect.succeed({} as any))); // Fallback

      // If we got a fallback (empty object) or invalid response, schema decode might fail
      // We'll wrap the decode in an Option or catch error
      const result = yield* Schema.decodeUnknown(BackendAnalyzeResponse)(response).pipe(
        Effect.catchAll(() => Effect.succeed({ aresHealth: Option.none() } as any))
      );

      const healthOpt = result.aresHealth;
      if (Option.isSome(healthOpt)) {
        const h = healthOpt.value as any;
        const newHealth = {
          tableName: relation.name,
          severity: (h.hjLevel === "Critical" ? "error" : h.hjLevel === "Warning" ? "warning" : "ok") as "ok" | "warning" | "error",
          message: h.hjMessage,
          suggestion: h.hjSuggestion,
        };

        yield* Ref.update(state, (s: WorkspaceState) => ({
          ...s,
          present: {
            ...s.present,
            health: [
              ...s.present.health.filter((lh) => lh.tableName !== relation.name),
              newHealth,
            ],
          }
        }));
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
