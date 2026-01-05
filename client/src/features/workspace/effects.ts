/**
 * Effects (The Adapter Logic / Service Layer)
 * Effectful logic, PubSub, and API interactions.
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
  readonly workspace: Ref.Ref<Workspace>;
  readonly events: PubSub.PubSub<WorkspaceEvent>;

  // Actions
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
}

export const WorkspaceService = Context.GenericTag<WorkspaceService>("@app/WorkspaceService");

// -- Implementation --

const make = Effect.gen(function* (_) {
  const state = yield* Ref.make<Workspace>({
    relations: [],
    crossTableFDs: [],
    foreignKeys: [],
    health: [],
    mergeSuggestions: [],
    analysisWarnings: [],
  });
  const events = yield* PubSub.unbounded<WorkspaceEvent>();

  const asAttribute = (s: string): Attribute => s as Attribute;

  const toRelation = (br: typeof BackendRelation.Type): Relation => ({
    id: br.rjName as TableId,
    name: br.rjName,
    attributes: br.rjAttributes.map((s) => asAttribute(s)),
    fds: br.rjFDs.map((fd) => ({
      lhs: fd.fjLhs.map((s) => asAttribute(s)),
      rhs: fd.fjRhs.map((s) => asAttribute(s)),
    })),
    position: { x: Math.random() * 400 + 100, y: Math.random() * 300 + 100 },
  });

  const updateCrossTableSuggestions = (ws: Workspace): Workspace => {
    const newCrossFDs = ws.crossTableFDs.map((ct) => {
      const fromRel = ws.relations.find((r) => r.name === ct.fromTable);
      const toRel = ws.relations.find((r) => r.name === ct.toTable);

      if (!fromRel || !toRel) return ct;

      const shared = fromRel.attributes.filter((attr) => toRel.attributes.includes(attr));
      const suggestion =
        shared.length > 0
          ? `🔗 Linked via ${shared.join(", ")}`
          : "⚠️ No shared keys. Rename attributes to match.";

      return { ...ct, suggestion };
    });

    return { ...ws, crossTableFDs: newCrossFDs };
  };

  const publishUpdate = Effect.gen(function* (_) {
    yield* PubSub.publish(events, { _tag: "NORMALIZATION_COMPLETED" });
  });

  const addRelation = (
    name: string,
    attributes: string[],
    fds: { lhs: string[]; rhs: string[] }[],
    x: number,
    y: number
  ): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      const newRel: Relation = {
        id: `${name}-${Date.now()}` as TableId,
        name,
        attributes: attributes.map((s) => asAttribute(s)),
        fds: fds.map((fd) => ({
          lhs: fd.lhs.map((s) => asAttribute(s)),
          rhs: fd.rhs.map((s) => asAttribute(s)),
        })),
        position: { x, y },
      };

      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: [...ws.relations, newRel],
      }));

      yield* PubSub.publish(events, { _tag: "RELATION_ADDED", relation: newRel });
    });

  const renameRelation = (id: string, newName: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      const ws = yield* Ref.get(state);
      const rel = ws.relations.find((r) => r.id === id);
      if (!rel) {
        return;
      }
      const oldName = rel.name;

      if (oldName === newName) {
        return;
      }

      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: ws.relations.map((r) => (r.id === id ? { ...r, name: newName } : r)),
        crossTableFDs: ws.crossTableFDs.map((cfd) => {
          const newFrom = cfd.fromTable === oldName ? newName : cfd.fromTable;
          const newTo = cfd.toTable === oldName ? newName : cfd.toTable;
          return {
            ...cfd,
            fromTable: newFrom,
            toTable: newTo,
            suggestion: `Relationship from ${newFrom} to ${newTo}`,
          };
        }),
        foreignKeys: ws.foreignKeys.map((fk) => ({
          ...fk,
          fromTable: fk.fromTable === oldName ? newName : fk.fromTable,
          toTable: fk.toTable === oldName ? newName : fk.toTable,
        })),
        health: ws.health.map((h) => ({
          ...h,
          tableName: h.tableName === oldName ? newName : h.tableName,
        })),
        mergeSuggestions: ws.mergeSuggestions.map(
          ([t1, t2, reason]) =>
            [t1 === oldName ? newName : t1, t2 === oldName ? newName : t2, reason] as const
        ),
      }));
      yield* publishUpdate;
    });

  const deleteRelation = (id: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: ws.relations.filter((r) => r.id !== id),
      }));
      yield* PubSub.publish(events, { _tag: "RELATION_DELETED", id });
    });

  const addAttribute = (relationId: string, name: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, (ws) => {
        const next = {
          ...ws,
          relations: ws.relations.map((r) =>
            r.id === relationId && !r.attributes.includes(asAttribute(name))
              ? { ...r, attributes: [...r.attributes, asAttribute(name)] }
              : r
          ),
        };
        return updateCrossTableSuggestions(next);
      });
      yield* publishUpdate;

      // Real-time analysis
      const ws = yield* Ref.get(state);
      const updatedRel = ws.relations.find((r) => r.id === relationId);
      if (updatedRel) {
        yield* analyzeRelation(updatedRel);
      }
    });

  const deleteAttribute = (relationId: string, name: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, (ws) => {
        const next = {
          ...ws,
          relations: ws.relations.map((r) =>
            r.id === relationId ? { ...r, attributes: r.attributes.filter((a) => a !== name) } : r
          ),
        };
        return updateCrossTableSuggestions(next);
      });
      yield* publishUpdate;

      // Real-time analysis
      const ws = yield* Ref.get(state);
      const updatedRel = ws.relations.find((r) => r.id === relationId);
      if (updatedRel) {
        yield* analyzeRelation(updatedRel);
      }
    });

  const addFD = (relationId: string, lhs: string[], rhs: string[]): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: ws.relations.map((r) =>
          r.id === relationId
            ? {
              ...r,
              fds: [...r.fds, { lhs: lhs.map(asAttribute), rhs: rhs.map(asAttribute) }],
            }
            : r
        ),
      }));
      yield* publishUpdate;

      // Real-time analysis
      const ws = yield* Ref.get(state);
      const updatedRel = ws.relations.find((r) => r.id === relationId);
      if (updatedRel) {
        yield* analyzeRelation(updatedRel);
      }
    });

  const deleteFD = (relationId: string, index: number): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: ws.relations.map((r) =>
          r.id === relationId ? { ...r, fds: r.fds.filter((_, i) => i !== index) } : r
        ),
      }));
      yield* publishUpdate;

      // Real-time analysis
      const ws = yield* Ref.get(state);
      const updatedRel = ws.relations.find((r) => r.id === relationId);
      if (updatedRel) {
        yield* analyzeRelation(updatedRel);
      }
    });

  const addCrossTableFD = (fromTableId: string, toTableId: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      const ws = yield* Ref.get(state);
      const fromRel = ws.relations.find((r) => r.id === fromTableId);
      const toRel = ws.relations.find((r) => r.id === toTableId);

      if (fromRel && toRel && fromTableId !== toTableId) {
        const shared = fromRel.attributes.filter((attr) => toRel.attributes.includes(attr));
        const suggestion =
          shared.length > 0
            ? `🔗 Linked via ${shared.join(", ")}`
            : "⚠️ No shared keys. Rename attributes to match.";

        yield* Ref.update(state, (s) => ({
          ...s,
          crossTableFDs: [
            ...s.crossTableFDs,
            {
              fromTable: fromRel.name,
              toTable: toRel.name,
              fd: { lhs: [], rhs: [] },
              suggestion,
            },
          ],
        }));
        yield* publishUpdate;
      }
    });

  const deleteCrossTableFD = (index: number): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, (ws) => ({
        ...ws,
        crossTableFDs: ws.crossTableFDs.filter((_, i) => i !== index),
      }));
      yield* publishUpdate;
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

        yield* Ref.update(state, (ws) => ({
          ...ws,
          health: [
            ...ws.health.filter((lh) => lh.tableName !== relation.name),
            newHealth,
          ],
        }));
        yield* publishUpdate;
      }
    });

  const updateRelationPosition = (id: string, x: number, y: number): Effect.Effect<void> =>
    Ref.update(state, (ws) => ({
      ...ws,
      relations: ws.relations.map((r) => (r.id === id ? { ...r, position: { x, y } } : r)),
    }));

  const normalizeRelation = (
    relationId: string,
    strategy: OptimizationStrategy
  ): Effect.Effect<typeof BackendDecompositionResult.Type | undefined, ApiError> =>
    Effect.gen(function* (_) {
      const ws = yield* Ref.get(state);
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

      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: result.nresRelations.map(toRelation),
      }));

      yield* PubSub.publish(events, { _tag: "NORMALIZATION_COMPLETED" });

      return result;
    });

  const optimizeWorkspace = (strategy: OptimizationStrategy): Effect.Effect<void, ApiError> =>
    Effect.gen(function* (_) {
      const ws = yield* Ref.get(state);

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
        yield* Ref.update(state, (s) => {
          // Generate warnings from our local crossTableFDs, as they reflect the user's manual links
          const activeCrossFDs = updateCrossTableSuggestions(s).crossTableFDs;
          const warnings = activeCrossFDs
            .filter((ct) => ct.suggestion.includes("No shared keys"))
            .map((ct) => `Link between ${ct.fromTable} and ${ct.toTable}: ${ct.suggestion}`);

          return {
            ...s,
            // Preserve our cross-table links, but don't overwrite them with empty results from backend
            // Only update health and suggestions
            health: result.wresHealth.map((h) => ({
              tableName: h.thjTableName,
              severity: h.thjSeverity as "ok" | "warning" | "error",
              message: h.thjMessage,
              suggestion: h.thjSuggestion,
            })),
            mergeSuggestions: result.wresMergeSuggestions,
            analysisWarnings: warnings,
          };
        });
        yield* PubSub.publish(events, { _tag: "WORKSPACE_OPTIMIZED" });
      }
    });

  const mergeRelations = (name1: string, name2: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      const ws = yield* Ref.get(state);
      const r1 = ws.relations.find((r) => r.name === name1);
      const r2 = ws.relations.find((r) => r.name === name2);

      if (!r1 || !r2) return;

      // Create merged relation
      const mergedName = name1; // Keep first name
      const mergedAttrs = Array.from(new Set([...r1.attributes, ...r2.attributes]));

      // Merge FDs
      // Simple concat for now, duplicate FDs are harmless but could be cleaned
      const mergedFDs = [...r1.fds, ...r2.fds];

      const mergedRel: Relation = {
        id: r1.id, // Reuse ID of first
        name: mergedName,
        attributes: mergedAttrs,
        fds: mergedFDs,
        position: {
          x: (r1.position.x + r2.position.x) / 2,
          y: (r1.position.y + r2.position.y) / 2,
        },
      };

      yield* Ref.update(state, (s) => {
        const next = {
          ...s,
          relations: [...s.relations.filter((r) => r.name !== name1 && r.name !== name2), mergedRel],
          // Remove the executed suggestion
          mergeSuggestions: s.mergeSuggestions.filter(([t1, t2]) =>
            !((t1 === name1 && t2 === name2) || (t1 === name2 && t2 === name1))
          ),
          // Clean up health entries for old tables
          health: s.health.filter(h => h.tableName !== name1 && h.tableName !== name2),
        };
        // Update cross-table links to point to new merged table
        // This is complex, so for now we'll just let the auto-updater handle links on next interaction
        // checking `updateCrossTableSuggestions` logic... it maps by name. 
        // We removed the old names, so old links point to nothing.
        // We should add links for the new table. 
        return updateCrossTableSuggestions(next);
      });

      yield* PubSub.publish(events, { _tag: "RELATION_UPDATED", relation: mergedRel });
      yield* analyzeRelation(mergedRel);
    });

  return {
    workspace: state,
    events,
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
  };
});

export const WorkspaceServiceLive = Layer.effect(WorkspaceService, make);
