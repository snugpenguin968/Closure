/**
 * Model.Optimizer
 * State for the optimization sidebar.
 */

import { Option } from "effect";
import { type OptimizationStrategy, type TreeNode, type TableHealth } from "../workspace/model";

// Analysis result from optimization
export interface Analysis {
  readonly tree: Option.Option<TreeNode>;
  readonly health: Option.Option<TableHealth>;
  readonly warnings: readonly string[];
}

// State is an interface since TreeNode/TableHealth are interfaces
export interface OptimizerState {
  readonly isOpen: boolean;
  readonly selectedStrategy: OptimizationStrategy;
  readonly targetRelationId: Option.Option<string>;
  readonly latestAnalysis: Option.Option<Analysis>;
}

// Initial state factory
export const createInitialOptimizerState = (): OptimizerState => ({
  isOpen: false,
  selectedStrategy: "3nf",
  targetRelationId: Option.none() as Option.Option<string>,
  latestAnalysis: Option.none() as Option.Option<Analysis>,
});
