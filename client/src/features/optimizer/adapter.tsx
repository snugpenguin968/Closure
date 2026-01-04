/**
 * Adapter.Optimizer
 * Connects Optimizer Service to View and Workspace Service.
 */

"use client";

import React, { useEffect, useState } from 'react';
import { Effect, Ref, Stream, Option } from "effect";
import { useOptimizerService } from "./context";
import { useWorkspaceService } from "../workspace/context";
import { OptimizerView } from "./view";
import { OptimizerState } from "./model";

export const OptimizerAdapter = () => {
    const optimizerService = useOptimizerService();
    const workspaceService = useWorkspaceService();

    const [state, setState] = useState<OptimizerState>({
        isOpen: false,
        selectedStrategy: '3nf',
        targetRelationId: Option.none(),
        latestAnalysis: Option.none()
    });

    // Hydrate state
    useEffect(() => {
        const program = Effect.gen(function* (_) {
            const updateUI = Effect.gen(function* (_) {
                const s = yield* Ref.get(optimizerService.state);
                yield* Effect.sync(() => setState(s));
            });

            yield* updateUI; // Initial

            // Subscribe to changes
            return yield* Stream.fromPubSub(optimizerService.changes).pipe(
                Stream.runForEach((s) => Effect.sync(() => setState(s)))
            );
        });

        const runPromise = Effect.runPromise(program);
        return () => { };
    }, [optimizerService]);

    const onStrategyChange = (strategy: any) => { // strict typing handled in view props
        Effect.runPromise(optimizerService.setStrategy(strategy));
    };

    const onOpenChange = (open: boolean) => {
        if (!open) Effect.runPromise(optimizerService.close);
    };

    const onAutoFix = () => {
        const targetId = Option.getOrNull(state.targetRelationId);

        if (targetId) {
            console.log("Auto-fixing relation:", targetId);
            Effect.runPromise(
                Effect.gen(function* (_) {
                    const result = yield* workspaceService.normalizeRelation(targetId, state.selectedStrategy);
                    if (result) {
                        // Map backend result to frontend state
                        // Define mappers here or import them. Since they are specific to this View model, local is fine.

                        // Recursive Tree Mapper
                        const mapTree = (node: any): any => ({
                            relation: { // generic Relation mapping, or simplified
                                id: node.tnRelation.name, // Simplified ID
                                name: node.tnRelation.name,
                                attributes: node.tnRelation.attributes, // Assuming string[] matches? No, need Attribute brand.
                                fds: [], // Simplified for visualization
                                position: { x: 0, y: 0 }
                            },
                            splitFD: Option.map(node.tnSplitFD, (fd: any) => ({
                                lhs: fd.lhs,
                                rhs: fd.rhs
                            })),
                            children: node.tnChildren.map(mapTree)
                        });

                        const health = Option.map(result.nresHealth, h => ({
                            tableName: targetId,
                            severity: h.level as any,
                            message: h.message,
                            suggestion: h.suggestion
                        }));

                        const tree = Option.map(result.nresTree, t => mapTree(t));

                        const analysis = {
                            tree,
                            health,
                            warnings: result.nresWarnings
                        };

                        yield* optimizerService.setLatestAnalysis(analysis);
                    }
                    // Close sidebar only if successful? Or keep open to show results?
                    // User requirement: "The UI animates...". Maybe keep open or allow user to interact.
                    // For now, let's keep it open so they see the tree. 
                    // yield* optimizerService.close; 
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
