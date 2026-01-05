/**
 * Model.Workspace (The Model)
 * Stores the state of every node, edge, and table boundary.
 * Effect Schema: Defines the Workspace structure.
 * Pure data definitions only.
 */

import * as Schema from "@effect/schema/Schema";
import { type Option } from "effect";

// -- Primitives --

export const Attribute = Schema.String.pipe(Schema.brand("Attribute"));
export type Attribute = Schema.Schema.Type<typeof Attribute>;

export const TableId = Schema.String.pipe(Schema.brand("TableId"));
export type TableId = Schema.Schema.Type<typeof TableId>;

// -- Functional Dependencies --

// -- Functional Dependencies --

export const FunctionalDependency = Schema.Struct({
    lhs: Schema.Array(Attribute),
    rhs: Schema.Array(Attribute),
});
export type FunctionalDependency = Schema.Schema.Type<typeof FunctionalDependency>;

export const CrossTableFD = Schema.Struct({
    fromTable: Schema.String,
    toTable: Schema.String,
    fd: FunctionalDependency,
    suggestion: Schema.String,
});
export type CrossTableFD = Schema.Schema.Type<typeof CrossTableFD>;

// -- Health & Diagnostics --

export const HealthSeverity = Schema.Literal("ok", "warning", "error");
export type HealthSeverity = Schema.Schema.Type<typeof HealthSeverity>;

export const TableHealth = Schema.Struct({
    tableName: Schema.String,
    severity: HealthSeverity,
    message: Schema.String,
    suggestion: Schema.String,
});
export type TableHealth = Schema.Schema.Type<typeof TableHealth>;

// -- Relations (Tables) --

export const Relation = Schema.Struct({
    id: TableId,
    name: Schema.String,
    attributes: Schema.Array(Attribute),
    fds: Schema.Array(FunctionalDependency),
    position: Schema.Struct({ x: Schema.Number, y: Schema.Number }), // Visual position
});
export type Relation = Schema.Schema.Type<typeof Relation>;

// -- Decomposition Tree (Recursion) --

export interface TreeNode {
    readonly relation: Relation;
    readonly splitFD: Option.Option<FunctionalDependency>;
    readonly children: ReadonlyArray<TreeNode>;
}

export const TreeNode: Schema.Schema<TreeNode, any> = Schema.suspend(() =>
    Schema.Struct({
        relation: Relation,
        splitFD: Schema.Option(FunctionalDependency),
        children: Schema.Array(TreeNode),
    })
);

// -- Foreign Keys --

export const ForeignKey = Schema.Struct({
    fromTable: Schema.String,
    fromAttribute: Schema.String,
    toTable: Schema.String,
    toAttribute: Schema.String,
});
export type ForeignKey = Schema.Schema.Type<typeof ForeignKey>;

// -- Workspace --

export const Workspace = Schema.Struct({
    relations: Schema.Array(Relation),
    crossTableFDs: Schema.Array(CrossTableFD),
    foreignKeys: Schema.Array(ForeignKey), // FK relationships after normalization
    health: Schema.Array(TableHealth),
    mergeSuggestions: Schema.Array(Schema.Tuple(Schema.String, Schema.String, Schema.String)),
    analysisWarnings: Schema.Array(Schema.String),
});
export type Workspace = Schema.Schema.Type<typeof Workspace>;

// -- Strategies --

export const OptimizationStrategy = Schema.Literal("bcnf", "3nf", "performance");
export type OptimizationStrategy = Schema.Schema.Type<typeof OptimizationStrategy>;

// -- API Types (matching Backend) --

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
    readonly tnRelation: BackendRelation;
    readonly tnSplitFD: Option.Option<{ readonly fjLhs: ReadonlyArray<string>; readonly fjRhs: ReadonlyArray<string> }>;
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
            ctjFD: Schema.Struct({ fjLhs: Schema.Array(Schema.String), fjRhs: Schema.Array(Schema.String) }),
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
