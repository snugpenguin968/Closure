/**
 * Adapter.HaskellSync (The Adapter)
 * Bridges the user's mouse and the Haskell logic.
 * Combines effectful logic and next/react hooks.
 */

"use client";

import React, { useEffect, useCallback } from 'react';
import { Effect, Ref, Stream } from "effect";
import {
    Node,
    Edge,
    useNodesState,
    useEdgesState,
    Connection,
    addEdge,
    MarkerType
} from 'reactflow';

import { useWorkspaceService } from "./context";
import { useOptimizerService } from "../optimizer/context";
import { WorkspaceCanvas } from "./view";
import { Workspace } from "./model";

// -- Adapter Component --

export const WorkspaceAdapter = () => {
    // Local React Flow state (for optimistic updates / dragging)
    const [nodes, setNodes, onNodesChange] = useNodesState([]);
    const [edges, setEdges, onEdgesChange] = useEdgesState([]);
    const [suggestions, setSuggestions] = React.useState<readonly (readonly [string, string, string])[]>([]);

    // Get services
    const service = useWorkspaceService();
    // Optional optimizer service
    const optimizerService = useOptimizerService();

    // Hydrate state from Service
    useEffect(() => {
        // Start the subscription
        const program = Effect.gen(function* (_) {

            const updateUI = Effect.gen(function* (_) {
                const ws = yield* Ref.get(service.workspace);

                // Handler closes over optimizerService to bridge Effect/React worlds
                const handler = (id: string) => () => {
                    Effect.runPromise(optimizerService.open(id));
                };

                const flow = mapStateToFlow(ws, handler);
                yield* Effect.sync(() => {
                    setNodes(flow.nodes);
                    setEdges(flow.edges);
                    setSuggestions(ws.mergeSuggestions);
                });
            });

            // Run initial update
            yield* updateUI;

            // Subscribe to future updates
            return yield* Stream.fromPubSub(service.events).pipe(
                Stream.runForEach(() => updateUI)
            );
        });

        const runPromise = Effect.runPromise(program);

        return () => {
            // cleanup if needed (cancel the fiber if we kept reference)
            // For now, simple runPromise is okay, but strictly we might want to cancel.
            // But since runPromise returns a Promise<void> and not a canceler (unless using runFork), 
            // we'll rely on GC or next render to ignore updates if mounted catch was improved.
        };
    }, [service, optimizerService, setNodes, setEdges]);

    const onConnect = useCallback((params: Connection) => {
        setEdges((eds) => addEdge(params, eds));
    }, [setEdges]);

    const onGlobalOptimize = () => {
        Effect.runPromise(service.optimizeWorkspace('3nf'));
    };

    // Handle Drag Stop -> Persist to Store
    const onNodeDragStop = useCallback((event: any, node: Node) => {
        if (!node.parentNode) {
            // Run effect using the service instance directly
            Effect.runPromise(service.updateRelationPosition(node.id, node.position.x, node.position.y));
        }
    }, [service]);

    return (
        <div className="w-full h-screen">
            <WorkspaceCanvas
                nodes={nodes}
                edges={edges}
                onNodesChange={onNodesChange}
                onEdgesChange={onEdgesChange}
                onConnect={onConnect}
                onNodeDragStop={onNodeDragStop}
                onGlobalOptimize={onGlobalOptimize}
                mergeSuggestions={suggestions}
            />
        </div>
    );
};

// -- Helper: Map Model to React Flow --

const mapStateToFlow = (workspace: Workspace, onNormalize: (id: string) => () => void): { nodes: Node[], edges: Edge[] } => {
    const nodes: Node[] = [];
    const edges: Edge[] = [];

    // Index health by table name for O(1) lookup
    const healthMap = new Map(workspace.health.map(h => [h.tableName, h]));

    workspace.relations.forEach(rel => {
        // 1. Table Group Node
        const tableHealth = healthMap.get(rel.name);
        nodes.push({
            id: rel.id,
            type: 'table',
            position: rel.position,
            data: {
                label: rel.name,
                onNormalize: onNormalize(rel.id),
                health: tableHealth ? { severity: tableHealth.severity, message: tableHealth.message } : undefined
            },
            style: { width: 250, height: 60 + rel.attributes.length * 40 }, // dynamic height
        });

        // 2. Attribute Nodes
        rel.attributes.forEach((attr, idx) => {
            const attrId = `${rel.id}-${attr}`;
            nodes.push({
                id: attrId,
                type: 'attribute',
                position: { x: 15, y: 60 + idx * 40 }, // Relative to parent
                parentNode: rel.id,
                extent: 'parent', // Constrain to parent
                data: { label: attr, isKey: false },
            });
        });

        // 3. Intra-table FDs (Edges)
        rel.fds.forEach((fd, i) => {
            // Render intra-table dependencies
            if (fd.lhs.length > 0 && fd.rhs.length > 0) {
                const sourceId = `${rel.id}-${fd.lhs[0]}`;
                const targetId = `${rel.id}-${fd.rhs[0]}`;
                edges.push({
                    id: `${rel.id}-fd-${i}`,
                    source: sourceId,
                    target: targetId,
                    animated: false,
                    style: { stroke: '#334155', strokeWidth: 2 },
                    markerEnd: { type: MarkerType.ArrowClosed, color: '#334155' },
                });
            }
        });
    });

    // 4. Cross-Table FDs (Dashed Edges)
    workspace.crossTableFDs.forEach((ctfd, i) => {
        // We need to find the node IDs for the attributes involved.
        // Since we don't have a direct map of Attribute -> NodeID, we might need to construct it or search.
        // But we know the naming convention: `${tableId}-${attribute}`.
        // Use table IDs from relation lookup.
        const fromRel = workspace.relations.find(r => r.name === ctfd.fromTable);
        const toRel = workspace.relations.find(r => r.name === ctfd.toTable);

        if (fromRel && toRel && ctfd.fd.lhs.length > 0 && ctfd.fd.rhs.length > 0) {
            const sourceId = `${fromRel.id}-${ctfd.fd.lhs[0]}`; // Simplified to first attr for now
            const targetId = `${toRel.id}-${ctfd.fd.rhs[0]}`;

            edges.push({
                id: `cross-${i}`,
                source: sourceId,
                target: targetId,
                animated: true,
                style: { stroke: '#f59e0b', strokeWidth: 2, strokeDasharray: '5,5' }, // Amber dashed line
                markerEnd: { type: MarkerType.ArrowClosed, color: '#f59e0b' },
                data: { tooltip: ctfd.suggestion } // Could add custom tooltip later
            });
        }
    });

    return { nodes, edges };
};
