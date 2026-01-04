/**
 * Effects (The Adapter Logic / Service Layer)
 * Effectful logic, PubSub, and API interactions.
 */

import { Effect, Context, Layer, Ref, PubSub, Stream, Console } from "effect";
import * as Schema from "@effect/schema/Schema";
import {
    Workspace,
    Relation,
    OptimizationStrategy,
    BackendRelation,
    BackendDecompositionResult,
    BackendWorkspaceResponse,
    FunctionalDependency,
    Attribute,
    TableId
} from "./model";

// -- Events --

export type WorkspaceEvent =
    | { _tag: "DECOMPOSITION_RESULT_RECEIVED"; result: typeof BackendDecompositionResult.Type }
    | { _tag: "RELATION_ADDED"; relation: Relation }
    | { _tag: "RELATION_UPDATED"; relation: Relation }
    | { _tag: "NORMALIZATION_COMPLETED" } // Added simple tag
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
    readonly addRelation: (name: string, attributes: string[], fds: { lhs: string[], rhs: string[] }[], x: number, y: number) => Effect.Effect<void>;
    readonly normalizeRelation: (relationId: string, strategy: OptimizationStrategy) => Effect.Effect<typeof BackendDecompositionResult.Type | undefined, ApiError>;
    readonly updateRelationPosition: (id: string, x: number, y: number) => Effect.Effect<void>;
    readonly optimizeWorkspace: (strategy: OptimizationStrategy) => Effect.Effect<void, ApiError>;
}

export const WorkspaceService = Context.GenericTag<WorkspaceService>("@app/WorkspaceService");

// -- Implementation --

const make = Effect.gen(function* (_) {
    const state = yield* Ref.make<Workspace>({
        relations: [],
        crossTableFDs: [],
        health: [],
        mergeSuggestions: []
    });
    const events = yield* PubSub.unbounded<WorkspaceEvent>();

    // Helper to brand strings safely (or validate)
    const asAttribute = (s: string) => s as Attribute;

    // Helper to convert BackendRelation to Relation
    const toRelation = (br: typeof BackendRelation.Type): Relation => {
        return {
            id: br.name as TableId, // Use name as ID for simplicity in this version
            name: br.name,
            attributes: br.attributes.map(s => asAttribute(s)),
            fds: br.fds.map(fd => ({
                lhs: fd.lhs.map(s => asAttribute(s)),
                rhs: fd.rhs.map(s => asAttribute(s))
            })),
            position: { x: Math.random() * 500 + 50, y: Math.random() * 300 + 50 } // Better random position
        };
    };

    const addRelation = (name: string, attributes: string[], fds: { lhs: string[], rhs: string[] }[], x: number, y: number) =>
        Effect.gen(function* (_) {
            const newRel: Relation = {
                id: name as TableId,
                name,
                attributes: attributes.map(s => asAttribute(s)),
                fds: fds.map(fd => ({
                    lhs: fd.lhs.map(s => asAttribute(s)),
                    rhs: fd.rhs.map(s => asAttribute(s))
                })),
                position: { x, y }
            };

            yield* Ref.update(state, ws => ({ // Changed workspace to state
                ...ws,
                relations: [...ws.relations, newRel]
            }));

            yield* PubSub.publish(events, { _tag: "RELATION_ADDED", relation: newRel });
            yield* Console.log(`Relation added: ${name}`);
        });

    const updateRelationPosition = (id: string, x: number, y: number) =>
        Ref.update(state, ws => ({ // Changed workspace to state
            ...ws,
            relations: ws.relations.map(r => r.id === id ? { ...r, position: { x, y } } : r)
        }));

    const normalizeRelation = (relationId: string, strategy: OptimizationStrategy) =>
        Effect.gen(function* (_) {
            const ws = yield* Ref.get(state); // Changed workspace to state
            const rel = ws.relations.find(r => r.id === relationId);

            if (!rel) {
                return undefined;
            }

            // Transform to backend format
            const payload = {
                nrRelation: {
                    rjName: rel.name,
                    rjAttributes: rel.attributes,
                    rjFDs: rel.fds.map(fd => ({ fjLhs: fd.lhs, fjRhs: fd.rhs }))
                },
                nrStrategy: strategy,
                nrIncludeTree: true
            };

            // API Call
            const response = yield* Effect.tryPromise({
                try: () => fetch("http://localhost:8080/api/normalize", {
                    method: "POST",
                    headers: { "Content-Type": "application/json" },
                    body: JSON.stringify(payload)
                }).then(res => res.json()),
                catch: (e) => new ApiError({ message: String(e) })
            });

            // Decode response
            const result = yield* Schema.decodeUnknown(BackendDecompositionResult)(response).pipe(
                Effect.mapError(e => new ApiError({ message: String(e) }))
            );

            // Process Normalization Result
            yield* Ref.update(state, ws => ({
                ...ws,
                relations: result.nresRelations.map(toRelation)
            }));

            // Publish event
            yield* PubSub.publish(events, { _tag: "NORMALIZATION_COMPLETED" });

            return result;
        });

    // -- Workspace Optimization (Multi-table) --
    // This is the "production" endpoint that returns cross-table FDs and health.
    const optimizeWorkspace = (strategy: OptimizationStrategy) =>
        Effect.gen(function* (_) {
            const ws = yield* Ref.get(state);

            // Call API /api/workspace
            const response = yield* Effect.tryPromise({
                try: () => fetch("http://localhost:8080/api/workspace", { // Ensure full URL or proxy
                    method: "POST",
                    headers: { "Content-Type": "application/json" },
                    body: JSON.stringify({
                        wrRelations: ws.relations.map(r => ({
                            rjName: r.name,
                            rjAttributes: r.attributes,
                            rjFDs: r.fds.map(fd => ({
                                fjLhs: fd.lhs,
                                fjRhs: fd.rhs
                            }))
                        })),
                        wrStrategy: strategy
                    })
                }).then(res => res.json()),
                catch: (e) => new ApiError({ message: String(e) })
            });


            const result = yield* Schema.decodeUnknown(BackendWorkspaceResponse)(response).pipe(
                Effect.mapError(e => new ApiError({ message: String(e) }))
            );

            if (result.wresSuccess) {
                // Update full workspace state with rich production data
                yield* Ref.update(state, s => ({
                    ...s,
                    crossTableFDs: result.wresCrossTableFDs.map(ct => ({
                        fromTable: ct.ctjFromTable,
                        toTable: ct.ctjToTable,
                        fd: { lhs: ct.ctjFD.lhs.map(s => Attribute.make(s)), rhs: ct.ctjFD.rhs.map(s => Attribute.make(s)) },
                        suggestion: ct.ctjSuggestion
                    })),
                    health: result.wresHealth.map(h => ({
                        tableName: h.thjTableName,
                        severity: h.thjSeverity as any,
                        message: h.thjMessage,
                        suggestion: h.thjSuggestion
                    })),
                    mergeSuggestions: result.wresMergeSuggestions
                }));
                yield* PubSub.publish(events, { _tag: "WORKSPACE_OPTIMIZED" });
            }
        });

    return {
        workspace: state, // Changed workspace to state
        events,
        addRelation,
        updateRelationPosition,
        normalizeRelation,
        optimizeWorkspace // Added
    };
});

export const WorkspaceServiceLive = Layer.effect(WorkspaceService, make);
