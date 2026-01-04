/**
 * Effects.Optimizer
 * Logic/State for the optimizer sidebar.
 */

import { Effect, Context, Layer, Ref, PubSub, Option } from "effect";
import { OptimizerState } from "./model";
import { OptimizationStrategy } from "../workspace/model";

// -- Service Definition --

export interface OptimizerService {
    readonly state: Ref.Ref<OptimizerState>;
    readonly changes: PubSub.PubSub<OptimizerState>;

    // Actions
    readonly open: (relationId: string) => Effect.Effect<void>;
    readonly close: Effect.Effect<void>;
    readonly setStrategy: (strategy: OptimizationStrategy) => Effect.Effect<void>;
    readonly setLatestAnalysis: (analysis: any) => Effect.Effect<void>; // Using any for now to avoid circular dep on Backend types, or we duplicate types
}

export const OptimizerService = Context.GenericTag<OptimizerService>("@app/OptimizerService");

// -- Implementation --

const make = Effect.gen(function* (_) {
    const state = yield* Ref.make<OptimizerState>({
        isOpen: false,
        selectedStrategy: "3nf", // Default
        targetRelationId: Option.none(),
        latestAnalysis: Option.none()
    });

    // PubSub for state changes
    const changes = yield* PubSub.unbounded<OptimizerState>();

    const updateState = (f: (s: OptimizerState) => OptimizerState) =>
        Effect.gen(function* (_) {
            const newState = yield* Ref.updateAndGet(state, f);
            yield* PubSub.publish(changes, newState);
        });

    const open = (relationId: string) =>
        updateState(s => ({
            ...s,
            isOpen: true,
            targetRelationId: Option.some(relationId)
        }));

    const close = updateState(s => ({ ...s, isOpen: false }));

    const setStrategy = (strategy: OptimizationStrategy) =>
        updateState(s => ({ ...s, selectedStrategy: strategy }));

    const setLatestAnalysis = (analysis: any) =>
        updateState(s => ({ ...s, latestAnalysis: Option.some(analysis) }));

    return {
        state,
        changes,
        open,
        close,
        setStrategy,
        setLatestAnalysis
    };
});

export const OptimizerServiceLive = Layer.effect(OptimizerService, make);
