"use client";

import React, { useEffect, useCallback } from "react";
import { Effect, Ref, Stream } from "effect";
import {
  type Node,
  type Edge,
  useNodesState,
  useEdgesState,
  type Connection,
  addEdge,
} from "reactflow";

import { mapStateToFlow } from "./view-model";
import { CanvasView } from "./view";

import { useWorkspaceService } from "../context";
import { useOptimizerService } from "../../optimizer/context";
import { type TableId, makeTableId } from "../model";

export const CanvasAdapter = (): React.ReactElement => {
  const service = useWorkspaceService();
  const optimizerService = useOptimizerService();

  const [nodes, setNodes, onNodesChange] = useNodesState([]);
  const [edges, setEdges, onEdgesChange] = useEdgesState([]);

  const onConnect = useCallback(
    (params: Connection) => {
      if (params.source && params.target) {
        Effect.runPromise(
          service.addCrossTableFD(makeTableId(params.source), makeTableId(params.target))
        );
      }
    },
    [service]
  );

  const onNodeDragStop = useCallback(
    (_event: React.MouseEvent, node: Node) => {
      Effect.runPromise(
        service.updateRelationPosition(makeTableId(node.id), node.position.x, node.position.y)
      );
    },
    [service]
  );

  // Hydrate state
  useEffect(() => {
    const program = Effect.gen(function* (_) {
      const updateUI = Effect.gen(function* (_) {
        const state = yield* Ref.get(service.state);
        const ws = state.present;
        const flow = mapStateToFlow(ws, service, optimizerService);

        yield* Effect.sync(() => {
          setNodes(flow.nodes);
          setEdges(flow.edges);
        });
      });

      yield* updateUI;
      return yield* Stream.fromPubSub(service.events).pipe(Stream.runForEach(() => updateUI));
    });
    Effect.runPromise(program);
  }, [service, optimizerService, setNodes, setEdges]);

  return (
    <CanvasView
      nodes={nodes}
      edges={edges}
      onNodesChange={onNodesChange}
      onEdgesChange={onEdgesChange}
      onConnect={onConnect}
      onNodeDragStop={onNodeDragStop}
    />
  );
};
