/**
 * Model.Workspace (The Model)
 * Stores the state of every node, edge, and table boundary.
 * Effect Schema: Defines the Workspace structure.
 * Pure data definitions only.
 *
 * DESIGN: TableId is a stable UUID generated on the frontend.
 * All references use TableId, not table name.
 * Backend uses names (stateless), frontend maps names↔IDs in effects.ts.
 */

import * as Schema from "@effect/schema/Schema";
import { HashMap, type Option } from "effect";

// -- Primitives --

export const Attribute = Schema.String.pipe(Schema.brand("Attribute"));
export type Attribute = Schema.Schema.Type<typeof Attribute>;

export interface History<T> {
  readonly past: ReadonlyArray<T>;
  readonly present: T;
  readonly future: ReadonlyArray<T>;
}

// TableId is a stable UUID, never changes after creation
export const TableId = Schema.String.pipe(Schema.brand("TableId"));
export type TableId = Schema.Schema.Type<typeof TableId>;

// -- Position --

export interface Position {
  readonly x: number;
  readonly y: number;
}

// -- Functional Dependencies --

export const FunctionalDependency = Schema.Struct({
  lhs: Schema.Array(Attribute),
  rhs: Schema.Array(Attribute),
});
export type FunctionalDependency = Schema.Schema.Type<typeof FunctionalDependency>;

// CrossTableFD uses TableIds
export const CrossTableFD = Schema.Struct({
  fromTableId: TableId,
  toTableId: TableId,
  fd: FunctionalDependency,
  suggestion: Schema.String,
});
export type CrossTableFD = Schema.Schema.Type<typeof CrossTableFD>;

// -- Health & Diagnostics --

export const HealthSeverity = Schema.Literal("ok", "warning", "error");
export type HealthSeverity = Schema.Schema.Type<typeof HealthSeverity>;

// TableHealth uses TableId
export const TableHealth = Schema.Struct({
  tableId: TableId,
  severity: HealthSeverity,
  message: Schema.String,
  suggestion: Schema.String,
});
export type TableHealth = Schema.Schema.Type<typeof TableHealth>;

// -- Relations (Tables) --

export interface Relation {
  readonly id: TableId;
  readonly name: string;
  readonly attributes: readonly Attribute[];
  readonly fds: readonly FunctionalDependency[];
  readonly position: Position;
}

// -- Decomposition Tree (Recursion) --

export interface TreeNode {
  readonly relation: Relation;
  readonly splitFD: Option.Option<FunctionalDependency>;
  readonly children: ReadonlyArray<TreeNode>;
}

// -- Foreign Keys --

export interface ForeignKey {
  readonly fromTableId: TableId;
  readonly fromAttribute: string;
  readonly toTableId: TableId;
  readonly toAttribute: string;
}

// -- Merge Suggestion --

export interface MergeSuggestion {
  readonly tableId1: TableId;
  readonly tableId2: TableId;
  readonly reason: string;
}

// -- Workspace (Runtime State with HashMap) --

export interface Workspace {
  readonly relations: HashMap.HashMap<TableId, Relation>;
  readonly crossTableFDs: readonly CrossTableFD[];
  readonly foreignKeys: readonly ForeignKey[];
  readonly health: readonly TableHealth[];
  readonly mergeSuggestions: readonly MergeSuggestion[];
  readonly analysisWarnings: readonly string[];
}

// -- Strategies --

export const OptimizationStrategy = Schema.Literal("bcnf", "3nf", "performance");
export type OptimizationStrategy = Schema.Schema.Type<typeof OptimizationStrategy>;

// -- API Types (matching Backend) --
// Backend uses names, not IDs. Frontend maps these in effects.ts.

export const BackendRelation = Schema.Struct({
  rjName: Schema.String,
  rjAttributes: Schema.Array(Schema.String),
  rjFDs: Schema.Array(
    Schema.Struct({ fjLhs: Schema.Array(Schema.String), fjRhs: Schema.Array(Schema.String) })
  ),
});

export type BackendRelation = Schema.Schema.Type<typeof BackendRelation>;

export const BackendHealth = Schema.Struct({
  hjLevel: Schema.String,
  hjMessage: Schema.String,
  hjViolations: Schema.Array(
    Schema.Struct({ fjLhs: Schema.Array(Schema.String), fjRhs: Schema.Array(Schema.String) })
  ),
  hjSuggestion: Schema.String,
});
export type BackendHealth = Schema.Schema.Type<typeof BackendHealth>;

export interface BackendTreeNode {
  readonly tnRelation: typeof BackendRelation.Type;
  readonly tnSplitFD: Option.Option<{
    readonly fjLhs: ReadonlyArray<string>;
    readonly fjRhs: ReadonlyArray<string>;
  }>;
  readonly tnChildren: ReadonlyArray<BackendTreeNode>;
}

export const BackendTreeNode: Schema.Schema<BackendTreeNode, any> = Schema.suspend(() =>
  Schema.Struct({
    tnRelation: BackendRelation,
    tnSplitFD: Schema.OptionFromNullOr(
      Schema.Struct({ fjLhs: Schema.Array(Schema.String), fjRhs: Schema.Array(Schema.String) })
    ),
    tnChildren: Schema.Array(BackendTreeNode),
  })
);

export const BackendDecompositionResult = Schema.Struct({
  nresSuccess: Schema.Boolean,
  nresRelations: Schema.Array(BackendRelation),
  nresTree: Schema.OptionFromNullOr(BackendTreeNode),
  nresForeignKeys: Schema.Array(Schema.Tuple(Schema.String, Schema.String)),
  nresWarnings: Schema.Array(Schema.String),
  nresHealth: Schema.OptionFromNullOr(BackendHealth),
  nresError: Schema.OptionFromNullOr(Schema.String),
});

export const BackendWorkspaceResponse = Schema.Struct({
  wresSuccess: Schema.Boolean,
  wresResults: Schema.Array(Schema.Tuple(Schema.String, Schema.Array(BackendRelation))),
  wresCrossTableFDs: Schema.Array(
    Schema.Struct({
      ctjFromTable: Schema.String,
      ctjToTable: Schema.String,
      ctjFD: Schema.Struct({
        fjLhs: Schema.Array(Schema.String),
        fjRhs: Schema.Array(Schema.String),
      }),
      ctjSuggestion: Schema.String,
    })
  ),
  wresHealth: Schema.Array(
    Schema.Struct({
      thjTableName: Schema.String,
      thjSeverity: Schema.String,
      thjMessage: Schema.String,
      thjSuggestion: Schema.String,
    })
  ),
  wresMergeSuggestions: Schema.Array(Schema.Tuple(Schema.String, Schema.String, Schema.String)),
  wresError: Schema.OptionFromNullOr(Schema.String),
});
