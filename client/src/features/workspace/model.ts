/**
 * Model.Workspace (The Model)
 * Stores the state of every node, edge, and table boundary.
 * Effect Schema: Defines the Workspace structure.
 * Pure data definitions only.
 */

import * as Schema from "@effect/schema/Schema";
import { Option } from "effect";

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
    suggestion: Schema.String
});
export type CrossTableFD = Schema.Schema.Type<typeof CrossTableFD>;

// -- Health & Diagnostics --

export const HealthSeverity = Schema.Literal("ok", "warning", "error");
export type HealthSeverity = Schema.Schema.Type<typeof HealthSeverity>;

export const TableHealth = Schema.Struct({
    tableName: Schema.String,
    severity: HealthSeverity,
    message: Schema.String,
    suggestion: Schema.String
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

export const TreeNode: Schema.Schema<TreeNode, any> = Schema.suspend(() => Schema.Struct({
    relation: Relation,
    splitFD: Schema.Option(FunctionalDependency),
    children: Schema.Array(TreeNode)
}));

// -- Workspace --

export const Workspace = Schema.Struct({
    relations: Schema.Array(Relation),
    crossTableFDs: Schema.Array(CrossTableFD),
    health: Schema.Array(TableHealth),
    mergeSuggestions: Schema.Array(Schema.Tuple(Schema.String, Schema.String, Schema.String)) // table1, table2, reason
});
export type Workspace = Schema.Schema.Type<typeof Workspace>;

// -- Strategies --

export const OptimizationStrategy = Schema.Literal("bcnf", "3nf", "performance");
export type OptimizationStrategy = Schema.Schema.Type<typeof OptimizationStrategy>;

// -- API Types (matching Backend) --

export const BackendRelation = Schema.Struct({
    name: Schema.String,
    attributes: Schema.Array(Schema.String),
    fds: Schema.Array(Schema.Struct({ lhs: Schema.Array(Schema.String), rhs: Schema.Array(Schema.String) }))
});

export const BackendHealth = Schema.Struct({
    level: Schema.String,
    message: Schema.String,
    violations: Schema.Array(Schema.Struct({ lhs: Schema.Array(Schema.String), rhs: Schema.Array(Schema.String) })),
    suggestion: Schema.String
});

export const BackendTreeNode: Schema.Schema<any> = Schema.suspend(() => Schema.Struct({
    tnRelation: BackendRelation,
    tnSplitFD: Schema.Option(Schema.Struct({ lhs: Schema.Array(Schema.String), rhs: Schema.Array(Schema.String) })),
    tnChildren: Schema.Array(BackendTreeNode)
}));

export const BackendDecompositionResult = Schema.Struct({
    nresSuccess: Schema.Boolean,
    nresRelations: Schema.Array(BackendRelation),
    nresTree: Schema.Option(BackendTreeNode),
    nresForeignKeys: Schema.Array(Schema.Tuple(Schema.String, Schema.String)),
    nresWarnings: Schema.Array(Schema.String),
    nresHealth: Schema.Option(BackendHealth),
    nresError: Schema.Option(Schema.String)
});

export const BackendWorkspaceResponse = Schema.Struct({
    wresSuccess: Schema.Boolean,
    wresResults: Schema.Array(Schema.Tuple(Schema.String, Schema.Array(BackendRelation))),
    wresCrossTableFDs: Schema.Array(Schema.Struct({
        ctjFromTable: Schema.String,
        ctjToTable: Schema.String,
        ctjFD: Schema.Struct({ lhs: Schema.Array(Schema.String), rhs: Schema.Array(Schema.String) }),
        ctjSuggestion: Schema.String
    })),
    wresHealth: Schema.Array(Schema.Struct({
        thjTableName: Schema.String,
        thjSeverity: Schema.String,
        thjMessage: Schema.String,
        thjSuggestion: Schema.String
    })),
    wresMergeSuggestions: Schema.Array(Schema.Tuple(Schema.String, Schema.String, Schema.String)),
    wresError: Schema.Option(Schema.String)
});
