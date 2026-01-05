/**
 * Adapter.HaskellSync (The Adapter)
 * Bridges the user's mouse and the Haskell logic.
 * Combines effectful logic and next/react hooks.
 */

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
    MarkerType,
} from "reactflow";

import { useWorkspaceService } from "./context";
import { useOptimizerService } from "../optimizer/context";
import { WorkspaceCanvas, type TableData } from "./view";
import { type Workspace, type Relation } from "./model";

// -- Adapter Component --

export const WorkspaceAdapter = (): React.ReactElement => {
    const [nodes, setNodes, onNodesChange] = useNodesState([]);
    const [edges, setEdges, onEdgesChange] = useEdgesState([]);
    const [suggestions, setSuggestions] = React.useState<
        readonly (readonly [string, string, string])[]
    >([]);

    const service = useWorkspaceService();
    const optimizerService = useOptimizerService();

    // Hydrate state from Service
    useEffect(() => {
        const program = Effect.gen(function* (_) {
            const updateUI = Effect.gen(function* (_) {
                const ws = yield* Ref.get(service.workspace);
                const flow = mapStateToFlow(ws, service, optimizerService);
                yield* Effect.sync(() => {
                    setNodes(flow.nodes);
                    setEdges(flow.edges);
                    setSuggestions(ws.mergeSuggestions);
                });
            });

            yield* updateUI;

            return yield* Stream.fromPubSub(service.events).pipe(Stream.runForEach(() => updateUI));
        });

        Effect.runPromise(program);

        return () => {
            // Cleanup handled by GC
        };
    }, [service, optimizerService, setNodes, setEdges]);

    const onConnect = useCallback(
        (params: Connection) => {
            // If connecting different tables, create a cross-table FD
            if (params.source && params.target && params.source !== params.target) {
                Effect.runPromise(service.addCrossTableFD(params.source, params.target));
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
            Effect.runPromise(service.updateRelationPosition(node.id, node.position.x, node.position.y));
        },
        [service]
    );

    const [isAnalyzing, setIsAnalyzing] = React.useState(false);

    const onGlobalOptimizeWithLoading = useCallback(() => {
        setIsAnalyzing(true);
        Effect.runPromise(service.optimizeWorkspace("3nf")).finally(() => {
            setIsAnalyzing(false);
        });
    }, [service]);

    const onEdgeClick = useCallback(
        (_event: React.MouseEvent, edge: Edge) => {
            // Delete cross-table FDs when clicking their edges
            if (edge.id.startsWith("cross-")) {
                const index = parseInt(edge.id.replace("cross-", ""), 10);
                if (!isNaN(index)) {
                    Effect.runPromise(service.deleteCrossTableFD(index));
                }
            }
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
                isAnalyzing={isAnalyzing}
                mergeSuggestions={suggestions}
            />
        </div>
    );
};

// -- Helper: Map Model to React Flow --

interface ServiceRef {
    renameRelation: (id: string, newName: string) => Effect.Effect<void>;
    addAttribute: (id: string, name: string) => Effect.Effect<void>;
    deleteAttribute: (id: string, name: string) => Effect.Effect<void>;
    addFD: (id: string, lhs: string[], rhs: string[]) => Effect.Effect<void>;
    deleteFD: (id: string, index: number) => Effect.Effect<void>;
}

interface OptimizerRef {
    open: (id: string) => Effect.Effect<void>;
}

const mapStateToFlow = (
    workspace: Workspace,
    service: ServiceRef,
    optimizerService: OptimizerRef
): { nodes: Node[]; edges: Edge[] } => {
    const nodes: Node[] = [];
    const edges: Edge[] = [];

    const healthMap = new Map(workspace.health.map((h) => [h.tableName, h]));

    workspace.relations.forEach((rel: Relation) => {
        const tableHealth = healthMap.get(rel.name);

        const tableData: TableData = {
            id: rel.id,
            name: rel.name,
            attributes: rel.attributes,
            fds: rel.fds,
            health: tableHealth
                ? { severity: tableHealth.severity, message: tableHealth.message }
                : undefined,
            onRename: (newName: string) => {
                Effect.runPromise(service.renameRelation(rel.id, newName));
            },
            onAddAttribute: (name: string) => {
                Effect.runPromise(service.addAttribute(rel.id, name));
            },
            onDeleteAttribute: (name: string) => {
                Effect.runPromise(service.deleteAttribute(rel.id, name));
            },
            onAddFD: (lhs: string[], rhs: string[]) => {
                Effect.runPromise(service.addFD(rel.id, lhs, rhs));
            },
            onDeleteFD: (index: number) => {
                Effect.runPromise(service.deleteFD(rel.id, index));
            },
            onOptimize: () => {
                Effect.runPromise(optimizerService.open(rel.id));
            },
        };

        nodes.push({
            id: rel.id,
            type: "table",
            position: rel.position,
            data: tableData,
        });
    });

    // Foreign Key edges (solid blue lines with FK label)
    workspace.foreignKeys.forEach((fk, i) => {
        const fromRel = workspace.relations.find((r) => r.name === fk.fromTable);
        const toRel = workspace.relations.find((r) => r.name === fk.toTable);

        if (fromRel && toRel) {
            edges.push({
                id: `fk-${i}`,
                source: fromRel.id,
                target: toRel.id,
                type: "smoothstep",
                animated: false,
                style: { stroke: "#3b82f6", strokeWidth: 2 },
                markerEnd: { type: MarkerType.ArrowClosed, color: "#3b82f6" },
                label: `FK: ${fk.fromAttribute}`,
                labelStyle: { fontSize: 9, fill: "#1d4ed8", fontWeight: 600 },
                labelBgStyle: { fill: "#dbeafe", fillOpacity: 0.95 },
            });
        }
    });

    // Cross-table FD edges (dashed amber lines)
    workspace.crossTableFDs.forEach((ctfd, i) => {
        const fromRel = workspace.relations.find((r) => r.name === ctfd.fromTable);
        const toRel = workspace.relations.find((r) => r.name === ctfd.toTable);

        if (fromRel && toRel) {
            edges.push({
                id: `cross-${i}`,
                source: fromRel.id,
                target: toRel.id,
                animated: true,
                style: { stroke: "#f59e0b", strokeWidth: 2, strokeDasharray: "5,5" },
                markerEnd: { type: MarkerType.ArrowClosed, color: "#f59e0b" },
                label: ctfd.suggestion,
                labelStyle: { fontSize: 9, fill: "#92400e" },
                labelBgStyle: { fill: "#fef3c7", fillOpacity: 0.9 },
            });
        }
    });

    return { nodes, edges };
};
