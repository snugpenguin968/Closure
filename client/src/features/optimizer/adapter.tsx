/**
 * Adapter.Optimizer
 * Connects Optimizer Service to View and Workspace Service.
 */

"use client";

import React, { useEffect, useState } from "react";
import { Effect, Ref, Stream, Option } from "effect";
import { useOptimizerService } from "./context";
import { useWorkspaceService } from "../workspace/context";
import { OptimizerView } from "./view";
import { type OptimizerState } from "./model";
import {
  type OptimizationStrategy,
  type TreeNode,
  type TableHealth,
  type Attribute,
  type TableId,
} from "../workspace/model";

export const OptimizerAdapter = (): React.ReactElement => {
  const optimizerService = useOptimizerService();
  const workspaceService = useWorkspaceService();

  const [state, setState] = useState<OptimizerState>({
    isOpen: false,
    selectedStrategy: "3nf",
    targetRelationId: Option.none(),
    latestAnalysis: Option.none(),
  });

  // Hydrate state
  useEffect(() => {
    const program = Effect.gen(function* (_) {
      const updateUI = Effect.gen(function* (_) {
        const s = yield* Ref.get(optimizerService.state);
        yield* Effect.sync(() => setState(s));
      });

      yield* updateUI;

      return yield* Stream.fromPubSub(optimizerService.changes).pipe(
        Stream.runForEach((s) => Effect.sync(() => setState(s)))
      );
    });

    Effect.runPromise(program);
    return () => {};
  }, [optimizerService]);

  const onStrategyChange = (strategy: OptimizationStrategy): void => {
    Effect.runPromise(optimizerService.setStrategy(strategy));
  };

  const onOpenChange = (open: boolean): void => {
    if (!open) {
      Effect.runPromise(optimizerService.close);
    }
  };

  const onAutoFix = (): void => {
    const targetId = Option.getOrNull(state.targetRelationId);

    if (targetId) {
      Effect.runPromise(
        Effect.gen(function* (_) {
          const result = yield* workspaceService.normalizeRelation(
            targetId,
            state.selectedStrategy
          );
          if (result) {
            // Map backend result to frontend TreeNode
            interface BackendTreeNode {
              tnRelation: {
                name: string;
                attributes: readonly string[];
                fds: readonly { lhs: readonly string[]; rhs: readonly string[] }[];
              };
              tnSplitFD: Option.Option<{ lhs: readonly string[]; rhs: readonly string[] }>;
              tnChildren: readonly BackendTreeNode[];
            }

            const mapTree = (node: BackendTreeNode): TreeNode => ({
              relation: {
                id: node.tnRelation.name as TableId,
                name: node.tnRelation.name,
                attributes: node.tnRelation.attributes.map((a) => a as Attribute),
                fds: node.tnRelation.fds.map((fd) => ({
                  lhs: fd.lhs.map((a) => a as Attribute),
                  rhs: fd.rhs.map((a) => a as Attribute),
                })),
                position: { x: 0, y: 0 },
              },
              splitFD: Option.map(node.tnSplitFD, (fd) => ({
                lhs: fd.lhs.map((a) => a as Attribute),
                rhs: fd.rhs.map((a) => a as Attribute),
              })),
              children: node.tnChildren.map(mapTree),
            });

            const health: Option.Option<TableHealth> = Option.map(result.nresHealth, (h) => ({
              tableName: targetId,
              severity: (h.level === "critical" ? "error" : h.level) as "ok" | "warning" | "error",
              message: h.message,
              suggestion: h.suggestion,
            }));

            const tree = Option.map(result.nresTree as Option.Option<BackendTreeNode>, mapTree);

            const analysis = {
              tree,
              health,
              warnings: result.nresWarnings,
            };

            yield* optimizerService.setLatestAnalysis(analysis);
          }
        })
      );
    }
  };

  const targetId = Option.getOrNull(state.targetRelationId) || undefined;

  return (
    <OptimizerView
      isOpen={state.isOpen}
      onOpenChange={onOpenChange}
      selectedStrategy={state.selectedStrategy}
      onStrategyChange={onStrategyChange}
      onAutoFix={onAutoFix}
      targetRelationId={targetId}
      latestAnalysis={state.latestAnalysis}
    />
  );
};
