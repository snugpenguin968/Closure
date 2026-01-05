/**
 * Effects (Service Layer)
 * Orchestrates API calls, PubSub events, and State Actions.
 * 
 * DESIGN: This layer handles the mapping between backend (name-based) 
 * and frontend (ID-based) representations.
 */

import { Effect, Context, Layer, Ref, PubSub, Option } from "effect";
import * as Schema from "@effect/schema/Schema";
import {
  type Workspace,
  type Relation,
  type OptimizationStrategy,
  type BackendRelation,
  type TableId,
  type MergeSuggestion,
  BackendDecompositionResult,
  BackendWorkspaceResponse,
  Attribute,
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
  | { _tag: "RELATION_DELETED"; id: TableId }
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

  // Helpers
  const updateAndNotify = (f: (s: WorkspaceState) => WorkspaceState): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, f);
      yield* PubSub.publish(events, { _tag: "STATE_UPDATED" });
    });

  // Build name→ID lookup from current workspace
  const buildNameToIdMap = (ws: Workspace): Map<string, TableId> =>
    new Map(ws.relations.map(r => [r.name, r.id]));

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

  const deleteRelation = (id: TableId) =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, Actions.deleteRelation(id));
      yield* PubSub.publish(events, { _tag: "STATE_UPDATED" });
    });

  const renameRelation = (id: TableId, newName: string) => updateAndNotify(Actions.renameRelation(id, newName));

  const addAttribute = (relationId: TableId, name: string) => updateAndNotify(Actions.addAttribute(relationId, name));

  const deleteAttribute = (relationId: TableId, name: string) => updateAndNotify(Actions.deleteAttribute(relationId, name));

  const addFD = (relationId: TableId, lhs: string[], rhs: string[]) => updateAndNotify(Actions.addFD(relationId, lhs, rhs));

  const deleteFD = (relationId: TableId, index: number) => updateAndNotify(Actions.deleteFD(relationId, index));

  const addCrossTableFD = (fromTableId: TableId, toTableId: TableId) => updateAndNotify(Actions.addCrossTableFD(fromTableId, toTableId));

  const deleteCrossTableFD = (index: number) => updateAndNotify(Actions.deleteCrossTableFD(index));

  const updateRelationPosition = (id: TableId, x: number, y: number) => Ref.update(state, Actions.updateRelationPosition(id, x, y));

  const mergeRelations = (id1: TableId, id2: TableId) => updateAndNotify(Actions.mergeRelations(id1, id2));

  // --- Complex Effects (API) ---

  const asAttribute = (s: string): Attribute => s as Attribute;

  const toRelation = (br: typeof BackendRelation.Type, existingId?: TableId): Relation => ({
    id: existingId ?? (crypto.randomUUID() as TableId),
    name: br.rjName,
    attributes: br.rjAttributes.map((s) => asAttribute(s)),
    fds: br.rjFDs.map((fd) => ({
      lhs: fd.fjLhs.map((s) => asAttribute(s)),
      rhs: fd.fjRhs.map((s) => asAttribute(s)),
    })),
    position: { x: 0, y: 0 },
  });

  const normalizeRelation = (
    relationId: TableId,
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
          ...result.nresRelations.map(br => toRelation(br))
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
        // Build name→ID map for backend→frontend translation
        const nameToId = buildNameToIdMap(ws);

        // Map backend health (name-based) to frontend (ID-based)
        const newHealth = result.wresHealth
          .map((h) => {
            const tableId = nameToId.get(h.thjTableName);
            if (!tableId) return null;
            return {
              tableId,
              severity: (h.thjSeverity === "Critical" ? "error" : h.thjSeverity === "Warning" ? "warning" : "ok") as "ok" | "warning" | "error",
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

        // Update state with results
        yield* Ref.update(state, Actions.setAnalysisResults(newHealth, newMergeSuggestions, []));

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

      const response = yield* Effect.tryPromise({
        try: () =>
          fetch("http://localhost:8080/api/analyze", {
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
          severity: (h.hjLevel === "Critical" ? "error" : h.hjLevel === "Warning" ? "warning" : "ok") as "ok" | "warning" | "error",
          message: h.hjMessage,
          suggestion: h.hjSuggestion,
        };

        yield* Ref.update(state, (s: WorkspaceState) => ({
          ...s,
          present: {
            ...s.present,
            health: [
              ...s.present.health.filter((lh) => lh.tableId !== relation.id),
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
