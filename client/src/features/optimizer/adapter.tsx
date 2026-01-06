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
  type FDId,
  DEFAULT_SQL_TYPE,
  makeTableId,
  makeAttribute,
  makeFDId,
  type SQLType,
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
            makeTableId(targetId),
            state.selectedStrategy
          );
          if (result) {
            // Map backend result to frontend TreeNode
            const mapTree = (node: BackendTreeNode): TreeNode => ({
              relation: {
                id: makeTableId(node.tnRelation.rjName),
                name: node.tnRelation.rjName,
                attributes: node.tnRelation.rjAttributes.map((attr) => ({
                  name: makeAttribute(attr.ajName),
                  sqlType: (attr.ajType || DEFAULT_SQL_TYPE) as SQLType,
                })),
                fds: node.tnRelation.rjFDs.map((fd) => ({
                  id: makeFDId(crypto.randomUUID()),
                  lhs: fd.fjLhs.map((a) => makeAttribute(a)),
                  rhs: fd.fjRhs.map((a) => makeAttribute(a)),
                })),
                position: { x: 0, y: 0 },
              },
              splitFD: Option.map(node.tnSplitFD, (fd) => ({
                id: makeFDId(crypto.randomUUID()),
                lhs: fd.fjLhs.map((a) => makeAttribute(a)),
                rhs: fd.fjRhs.map((a) => makeAttribute(a)),
              })),
              children: node.tnChildren.map(mapTree),
            });

            const health: Option.Option<TableHealth> = Option.map(result.nresHealth, (h) => ({
              tableId: makeTableId(targetId),
              severity: (h.hjLevel === "critical" ? "error" : h.hjLevel === "warning" ? "warning" : "ok"),
              message: h.hjMessage,
              suggestion: h.hjSuggestion,
            }));

            // nresTree is already an Option per Schema definition
            const tree = Option.map(result.nresTree, mapTree);

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
