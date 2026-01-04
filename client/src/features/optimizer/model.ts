/**
 * Model.Optimizer
 * State for the optimization sidebar.
 */

import * as Schema from "@effect/schema/Schema";
import { OptimizationStrategy, TreeNode, TableHealth } from "../workspace/model";

export const OptimizerState = Schema.Struct({
    isOpen: Schema.Boolean,
    selectedStrategy: OptimizationStrategy,
    targetRelationId: Schema.Option(Schema.String), // The table being optimized
    latestAnalysis: Schema.Option(Schema.Struct({
        tree: Schema.Option(TreeNode),
        health: Schema.Option(TableHealth),
        warnings: Schema.Array(Schema.String)
    }))
});

export type OptimizerState = Schema.Schema.Type<typeof OptimizerState>;
