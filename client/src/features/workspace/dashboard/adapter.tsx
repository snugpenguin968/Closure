"use client";

import React, { useEffect, useCallback, useState } from "react";
import { Effect, Ref, Stream, HashMap } from "effect";
import { useWorkspaceService } from "../context";
import { type TableId, type OptimizationStrategy } from "../model";
import { DashboardView } from "./view";
import { mapSuggestionsToDisplay, type MergeSuggestionDisplay } from "./view-model";

export const DashboardAdapter = (): React.ReactElement => {
  const service = useWorkspaceService();
  const [suggestions, setSuggestions] = useState<readonly MergeSuggestionDisplay[]>([]);
  const [warnings, setWarnings] = useState<readonly string[]>([]);
  const [isAnalyzing, setIsAnalyzing] = useState(false);

  // Sync state (Suggestions & Warnings)
  useEffect(() => {
    const program = Effect.gen(function* (_) {
      const updateUI = Effect.gen(function* (_) {
        const state = yield* Ref.get(service.state);
        const ws = state.present;
        setSuggestions(mapSuggestionsToDisplay(ws));
        setWarnings(ws.analysisWarnings);
      });

      yield* updateUI;
      return yield* Stream.fromPubSub(service.events).pipe(Stream.runForEach(() => updateUI));
    });
    Effect.runPromise(program);
  }, [service]);

  // Sync analysis status (Loading state)
  useEffect(() => {
    const program = Stream.fromPubSub(service.events).pipe(
      Stream.runForEach((event) =>
        Effect.sync(() => {
          if (event._tag === "ANALYSIS_STARTED") {
            setIsAnalyzing(true);
          } else if (event._tag === "ANALYSIS_COMPLETED") {
            setIsAnalyzing(false);
          }
        })
      )
    );
    Effect.runPromise(program);
  }, [service]);

  const onOptimize = useCallback(
    (strategy: "bcnf" | "3nf") => {
      Effect.runPromise(service.optimizeWorkspace(strategy as OptimizationStrategy));
    },
    [service]
  );

  const onExportSQL = useCallback(() => {
    Effect.runPromise(
      Effect.gen(function* (_) {
        const state = yield* Ref.get(service.state);
        const { generateSQL } = yield* Effect.promise(() => import("../sql-generator"));
        const sql = generateSQL(state.present);

        const blob = new Blob([sql], { type: "text/plain" });
        const url = URL.createObjectURL(blob);
        const a = document.createElement("a");
        a.href = url;
        a.download = "schema.sql";
        a.click();
        URL.revokeObjectURL(url);
      })
    );
  }, [service]);

  const onMerge = useCallback(
    (id1: string, id2: string) => {
      Effect.runPromise(service.mergeRelations(id1 as TableId, id2 as TableId));
    },
    [service]
  );

  const onAddTable = useCallback(() => {
    const name = `Table${Math.floor(Math.random() * 1000)}`;
    Effect.runPromise(
      service.addRelation(name, [], [], 200 + Math.random() * 200, 150 + Math.random() * 100)
    );
  }, [service]);

  return (
    <DashboardView
      onOptimize={onOptimize}
      onAddTable={onAddTable}
      onExportSQL={onExportSQL}
      suggestions={suggestions}
      isAnalyzing={isAnalyzing}
      analysisWarnings={warnings}
      onMerge={onMerge}
    />
  );
};
