/**
 * Workspace Errors
 * Typed error classes using Effect's Data.TaggedError.
 * Enables exhaustive error handling with type safety.
 */

import { Data } from "effect";

// -- Network Errors --

/** Failed to reach the backend API */
export class NetworkError extends Data.TaggedError("NetworkError")<{
  readonly url: string;
  readonly cause: unknown;
}> {}

// -- Backend Errors --

/** Backend returned an error response */
export class BackendError extends Data.TaggedError("BackendError")<{
  readonly endpoint: string;
  readonly message: string;
}> {}

// -- Schema/Validation Errors --

/** Failed to decode backend response */
export class DecodeError extends Data.TaggedError("DecodeError")<{
  readonly schema: string;
  readonly message: string;
}> {}

/** User input validation failed */
export class ValidationError extends Data.TaggedError("ValidationError")<{
  readonly field: string;
  readonly message: string;
}> {}

// -- Entity Errors --

/** Entity not found in state */
export class NotFoundError extends Data.TaggedError("NotFoundError")<{
  readonly entityType: "relation" | "crossTableFD" | "foreignKey" | "fd";
  readonly id: string;
}> {}

// -- Union Type --

export type WorkspaceError =
  | NetworkError
  | BackendError
  | DecodeError
  | ValidationError
  | NotFoundError;
