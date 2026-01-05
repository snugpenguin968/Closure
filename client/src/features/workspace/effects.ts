/**
 * Effects (The Adapter Logic / Service Layer)
 * Effectful logic, PubSub, and API interactions.
 */

import { Effect, Context, Layer, Ref, PubSub, Console } from "effect";
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
} from "./model";

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
}) {}

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
  });
  const events = yield* PubSub.unbounded<WorkspaceEvent>();

  const asAttribute = (s: string): Attribute => s as Attribute;

  const toRelation = (br: typeof BackendRelation.Type): Relation => ({
    id: br.name as TableId,
    name: br.name,
    attributes: br.attributes.map((s) => asAttribute(s)),
    fds: br.fds.map((fd) => ({
      lhs: fd.lhs.map((s) => asAttribute(s)),
      rhs: fd.rhs.map((s) => asAttribute(s)),
    })),
    position: { x: Math.random() * 400 + 100, y: Math.random() * 300 + 100 },
  });

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
      yield* Console.log(`Relation added: ${name}`);
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
        crossTableFDs: ws.crossTableFDs.map((cfd) => ({
          ...cfd,
          fromTable: cfd.fromTable === oldName ? newName : cfd.fromTable,
          toTable: cfd.toTable === oldName ? newName : cfd.toTable,
        })),
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
      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: ws.relations.map((r) =>
          r.id === relationId ? { ...r, attributes: [...r.attributes, asAttribute(name)] } : r
        ),
      }));
      yield* publishUpdate;
    });

  const deleteAttribute = (relationId: string, name: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      yield* Ref.update(state, (ws) => ({
        ...ws,
        relations: ws.relations.map((r) =>
          r.id === relationId ? { ...r, attributes: r.attributes.filter((a) => a !== name) } : r
        ),
      }));
      yield* publishUpdate;
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
    });

  const addCrossTableFD = (fromTableId: string, toTableId: string): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      const ws = yield* Ref.get(state);
      const fromRel = ws.relations.find((r) => r.id === fromTableId);
      const toRel = ws.relations.find((r) => r.id === toTableId);

      if (fromRel && toRel && fromTableId !== toTableId) {
        yield* Ref.update(state, (s) => ({
          ...s,
          crossTableFDs: [
            ...s.crossTableFDs,
            {
              fromTable: fromRel.name,
              toTable: toRel.name,
              fd: { lhs: [], rhs: [] }, // Empty FD, user can define later
              suggestion: `Relationship from ${fromRel.name} to ${toRel.name}`,
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
        yield* Ref.update(state, (s) => ({
          ...s,
          crossTableFDs: result.wresCrossTableFDs.map((ct) => ({
            fromTable: ct.ctjFromTable,
            toTable: ct.ctjToTable,
            fd: {
              lhs: ct.ctjFD.lhs.map((s) => Attribute.make(s)),
              rhs: ct.ctjFD.rhs.map((s) => Attribute.make(s)),
            },
            suggestion: ct.ctjSuggestion,
          })),
          health: result.wresHealth.map((h) => ({
            tableName: h.thjTableName,
            severity: h.thjSeverity as "ok" | "warning" | "error",
            message: h.thjMessage,
            suggestion: h.thjSuggestion,
          })),
          mergeSuggestions: result.wresMergeSuggestions,
        }));
        yield* PubSub.publish(events, { _tag: "WORKSPACE_OPTIMIZED" });
      }
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
  };
});

export const WorkspaceServiceLive = Layer.effect(WorkspaceService, make);
