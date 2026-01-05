/**
 * Effects.Optimizer
 * Logic/State for the optimizer sidebar.
 */

import { Effect, Context, Layer, Ref, PubSub, Option } from "effect";
import { type OptimizerState, type Analysis } from "./model";
import { type OptimizationStrategy } from "../workspace/model";

// -- Service Definition --

export interface OptimizerService {
  readonly state: Ref.Ref<OptimizerState>;
  readonly changes: PubSub.PubSub<OptimizerState>;

  // Actions
  readonly open: (relationId: string) => Effect.Effect<void>;
  readonly close: Effect.Effect<void>;
  readonly setStrategy: (strategy: OptimizationStrategy) => Effect.Effect<void>;
  readonly setLatestAnalysis: (analysis: Analysis) => Effect.Effect<void>;
}

export const OptimizerService = Context.GenericTag<OptimizerService>("@app/OptimizerService");

// -- Implementation --

const make = Effect.gen(function* (_) {
  const state = yield* Ref.make<OptimizerState>({
    isOpen: false,
    selectedStrategy: "3nf",
    targetRelationId: Option.none(),
    latestAnalysis: Option.none(),
  });

  const changes = yield* PubSub.unbounded<OptimizerState>();

  const updateState = (f: (s: OptimizerState) => OptimizerState): Effect.Effect<void> =>
    Effect.gen(function* (_) {
      const newState = yield* Ref.updateAndGet(state, f);
      yield* PubSub.publish(changes, newState);
    });

  const open = (relationId: string): Effect.Effect<void> =>
    updateState((s) => ({
      ...s,
      isOpen: true,
      targetRelationId: Option.some(relationId),
    }));

  const close: Effect.Effect<void> = updateState((s) => ({ ...s, isOpen: false }));

  const setStrategy = (strategy: OptimizationStrategy): Effect.Effect<void> =>
    updateState((s) => ({ ...s, selectedStrategy: strategy }));

  const setLatestAnalysis = (analysis: Analysis): Effect.Effect<void> =>
    updateState((s) => ({ ...s, latestAnalysis: Option.some(analysis) }));

  return {
    state,
    changes,
    open,
    close,
    setStrategy,
    setLatestAnalysis,
  };
});

export const OptimizerServiceLive = Layer.effect(OptimizerService, make);
