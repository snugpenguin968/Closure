/**
 * Adapter.HaskellSync (The Adapter)
 * Bridges the user's mouse and the Haskell logic.
 * Combines effectful logic and next/react hooks.
 *
 * DESIGN: Resolves IDs to display names for the View.
 * Subscribes to granular events for efficient updates.
 * View remains dumb - receives resolved data, fires callbacks with IDs.
 */

"use client";

import React, { useEffect, useCallback } from "react";
import { Effect, Ref, Stream, HashMap } from "effect";
import {
  type Node,
  type Edge,
  useNodesState,
  useEdgesState,
  type Connection,
  addEdge,
  MarkerType,
} from "reactflow";

import {
  mapStateToFlow,
  mapSuggestionsToDisplay,
  type TableData,
  type MergeSuggestionDisplay,
} from "./view-model";

import { useWorkspaceService } from "./context";
import { useOptimizerService } from "../optimizer/context";
import { WorkspaceCanvas } from "./view";
import {
  type Workspace,
  type Relation,
  type TableId,
  type CrossTableFDId,
  type FDId,
} from "./model";

// -- Adapter Component --

export const WorkspaceAdapter = (): React.ReactElement => {
  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);
  const [suggestions, setSuggestions] = React.useState<MergeSuggestionDisplay[]>([]);
  const [warnings, setWarnings] = React.useState<readonly string[]>([]);

  const service = useWorkspaceService();
  const optimizerService = useOptimizerService();

  // Hydrate state from Service
  useEffect(() => {
    const program = Effect.gen(function* (_) {
      const updateUI = Effect.gen(function* (_) {
        const state = yield* Ref.get(service.state);
        const ws = state.present;
        const flow = mapStateToFlow(ws, service, optimizerService);

        // Resolve merge suggestions: IDs → display names
        // Resolve merge suggestions
        const resolvedSuggestions = mapSuggestionsToDisplay(ws);

        yield* Effect.sync(() => {
          setNodes(flow.nodes);
          setEdges(flow.edges);
          setSuggestions(resolvedSuggestions);
          setWarnings(ws.analysisWarnings);
        });
      });

      yield* updateUI;

      // Subscribe to all events and update UI
      // In future, could optimize by handling specific events differently
      return yield* Stream.fromPubSub(service.events).pipe(Stream.runForEach(() => updateUI));
    });

    Effect.runPromise(program);

    const handleKeyDown = (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === "z") {
        if (e.shiftKey) {
          Effect.runPromise(service.redo);
        } else {
          Effect.runPromise(service.undo);
        }
      }
    };

    window.addEventListener("keydown", handleKeyDown);

    return () => {
      window.removeEventListener("keydown", handleKeyDown);
    };
  }, [service, optimizerService, setNodes, setEdges]);

  const onConnect = useCallback(
    (params: Connection) => {
      if (params.source && params.target && params.source !== params.target) {
        Effect.runPromise(
          service.addCrossTableFD(params.source as TableId, params.target as TableId)
        );
      }
      setEdges((eds) => addEdge(params, eds));
    },
    [setEdges, service]
  );

  const onAddTable = useCallback(() => {
    const name = `Table${Math.floor(Math.random() * 1000)}`;
    Effect.runPromise(
      service.addRelation(name, [], [], 200 + Math.random() * 200, 150 + Math.random() * 100)
    );
  }, [service]);

  const onNodeDragStop = useCallback(
    (_event: unknown, node: Node) => {
      Effect.runPromise(
        service.updateRelationPosition(node.id as TableId, node.position.x, node.position.y)
      );
    },
    [service]
  );

  const [isAnalyzing, setIsAnalyzing] = React.useState(false);

  // Subscribe to analysis events for loading state
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
  }, [service.events]);

  const onGlobalOptimizeWithLoading = useCallback(() => {
    Effect.runPromise(service.optimizeWorkspace("3nf")).catch(() => {
      setIsAnalyzing(false);
    });
  }, [service]);

  const onEdgeClick = useCallback(
    (_event: React.MouseEvent, edge: Edge) => {
      // Edge IDs are now in format "cross-{CrossTableFDId}"
      if (edge.id.startsWith("cross-")) {
        const crossTableFDId = edge.id.replace("cross-", "") as CrossTableFDId;
        Effect.runPromise(service.deleteCrossTableFD(crossTableFDId));
      }
    },
    [service]
  );

  const onExportSQL = useCallback(() => {
    const currentState = Effect.runSync(Ref.get(service.state));
    const ws = currentState.present;

    import("./sql-generator").then(({ generateSQL }) => {
      const sql = generateSQL(ws);

      const blob = new Blob([sql], { type: "text/plain" });
      const url = URL.createObjectURL(blob);
      const a = document.createElement("a");
      a.href = url;
      a.download = "schema.sql";
      a.click();
      URL.revokeObjectURL(url);
    });
  }, [service]);

  const onMerge = useCallback(
    (id1: string, id2: string) => {
      Effect.runPromise(service.mergeRelations(id1 as TableId, id2 as TableId));
    },
    [service]
  );

  return (
    <div className="w-full h-screen">
      <WorkspaceCanvas
        nodes={nodes}
        edges={edges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onConnect={onConnect}
        onNodeDragStop={onNodeDragStop}
        onEdgeClick={onEdgeClick}
        onGlobalOptimize={onGlobalOptimizeWithLoading}
        onAddTable={onAddTable}
        onExportSQL={onExportSQL}
        isAnalyzing={isAnalyzing}
        mergeSuggestions={suggestions}
        onMergeRelations={onMerge}
        analysisWarnings={warnings}
      />
    </div>
  );
};
