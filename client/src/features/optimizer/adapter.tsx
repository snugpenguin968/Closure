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
  type BackendTreeNode,
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
    return () => { };
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
            const mapTree = (node: BackendTreeNode): TreeNode => ({
              relation: {
                id: node.tnRelation.rjName as TableId,
                name: node.tnRelation.rjName,
                attributes: node.tnRelation.rjAttributes.map((a) => a as Attribute),
                fds: node.tnRelation.rjFDs.map((fd) => ({
                  lhs: fd.fjLhs.map((a) => a as Attribute),
                  rhs: fd.fjRhs.map((a) => a as Attribute),
                })),
                position: { x: 0, y: 0 },
              },
              splitFD: Option.map(node.tnSplitFD, (fd) => ({
                lhs: fd.fjLhs.map((a) => a as Attribute),
                rhs: fd.fjRhs.map((a) => a as Attribute),
              })),
              children: node.tnChildren.map(mapTree),
            });

            const health: Option.Option<TableHealth> = Option.map(result.nresHealth, (h) => ({
              tableName: targetId,
              severity: (h.hjLevel === "critical" ? "error" : h.hjLevel) as "ok" | "warning" | "error",
              message: h.hjMessage,
              suggestion: h.hjSuggestion,
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
