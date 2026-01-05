/**
 * API Infrastructure Layer
 * Handles all HTTP communication and schema validation.
 * Pure Effect output - no business logic.
 *
 * Design:
 * - Uses structural interfaces for dependencies if needed.
 * - Returns typed errors (NetworkError, DecodeError).
 */

import { Effect } from "effect";
import * as Schema from "@effect/schema/Schema";
import { API_BASE_URL } from "./config";
import { NetworkError, DecodeError, type WorkspaceError } from "./errors";
import {
  type TableId,
  type OptimizationStrategy,
  type Relation,
  BackendDecompositionResult,
  BackendWorkspaceResponse,
  BackendAnalyzeResponse,
} from "./model";

// -- Interfaces --

export interface NormalizeRequest {
  readonly relation: Relation;
  readonly strategy: OptimizationStrategy;
}

export interface WorkspaceApi {
  readonly normalize: (
    req: NormalizeRequest
  ) => Effect.Effect<typeof BackendDecompositionResult.Type, WorkspaceError>;

  readonly optimize: (
    relations: Relation[],
    strategy: OptimizationStrategy
  ) => Effect.Effect<typeof BackendWorkspaceResponse.Type, WorkspaceError>;

  readonly analyze: (
    relation: Relation
  ) => Effect.Effect<typeof BackendAnalyzeResponse.Type, WorkspaceError>;
}

// -- Implementation --

const normalize = (
  req: NormalizeRequest
): Effect.Effect<typeof BackendDecompositionResult.Type, WorkspaceError> =>
  Effect.gen(function* (_) {
    const payload = {
      nrRelation: {
        rjName: req.relation.name,
        rjAttributes: req.relation.attributes,
        rjFDs: req.relation.fds.map((fd) => ({ fjLhs: fd.lhs, fjRhs: fd.rhs })),
      },
      nrStrategy: req.strategy,
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

    return yield* Schema.decodeUnknown(BackendDecompositionResult)(response).pipe(
      Effect.mapError(
        (e) => new DecodeError({ schema: "BackendDecompositionResult", message: String(e) })
      )
    );
  });

const optimize = (
  relations: Relation[],
  strategy: OptimizationStrategy
): Effect.Effect<typeof BackendWorkspaceResponse.Type, WorkspaceError> =>
  Effect.gen(function* (_) {
    const url = `${API_BASE_URL}/api/workspace`;
    const response = yield* Effect.tryPromise({
      try: () =>
        fetch(url, {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            wrRelations: relations.map((r) => ({
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

    return yield* Schema.decodeUnknown(BackendWorkspaceResponse)(response).pipe(
      Effect.mapError(
        (e) => new DecodeError({ schema: "BackendWorkspaceResponse", message: String(e) })
      )
    );
  });

const analyze = (
  relation: Relation
): Effect.Effect<typeof BackendAnalyzeResponse.Type, WorkspaceError> =>
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
      catch: (e) => new NetworkError({ url, cause: e }),
    });

    return yield* Schema.decodeUnknown(BackendAnalyzeResponse)(response).pipe(
      Effect.mapError(
        (e) => new DecodeError({ schema: "BackendAnalyzeResponse", message: String(e) })
      )
    );
  });

export const api: WorkspaceApi = {
  normalize,
  optimize,
  analyze,
};
