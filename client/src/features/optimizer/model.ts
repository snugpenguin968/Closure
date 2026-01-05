/**
 * Model.Optimizer
 * State for the optimization sidebar.
 */

import * as Schema from "@effect/schema/Schema";
import { OptimizationStrategy, TreeNode, TableHealth } from "../workspace/model";
import { type Option } from "effect";

// Analysis result from optimization
export interface Analysis {
  readonly tree: Option.Option<TreeNode>;
  readonly health: Option.Option<TableHealth>;
  readonly warnings: readonly string[];
}

export const OptimizerState = Schema.Struct({
  isOpen: Schema.Boolean,
  selectedStrategy: OptimizationStrategy,
  targetRelationId: Schema.Option(Schema.String),
  latestAnalysis: Schema.Option(
    Schema.Struct({
      tree: Schema.Option(TreeNode),
      health: Schema.Option(TableHealth),
      warnings: Schema.Array(Schema.String),
    })
  ),
});

export type OptimizerState = Schema.Schema.Type<typeof OptimizerState>;
