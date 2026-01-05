/**
 * Adapter.HaskellSync (The Adapter)
 * Bridges the user's mouse and the Haskell logic.
 * Combines effectful logic and next/react hooks.
 * 
 * DESIGN: Resolves IDs to display names for the View.
 * View remains dumb - receives resolved data, fires callbacks with IDs.
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
import { WorkspaceCanvas, type TableData, type MergeSuggestionDisplay } from "./view";
import { type Workspace, type Relation, type TableId } from "./model";

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
                const idToName = new Map(ws.relations.map(r => [r.id, r.name]));
                const resolvedSuggestions: MergeSuggestionDisplay[] = ws.mergeSuggestions
                    .map(s => {
                        const name1 = idToName.get(s.tableId1);
                        const name2 = idToName.get(s.tableId2);
                        if (!name1 || !name2) return null;
                        return {
                            id1: s.tableId1,
                            id2: s.tableId2,
                            name1,
                            name2,
                            reason: s.reason,
                        };
                    })
                    .filter((s): s is NonNullable<typeof s> => s !== null);

                yield* Effect.sync(() => {
                    setNodes(flow.nodes);
                    setEdges(flow.edges);
                    setSuggestions(resolvedSuggestions);
                    setWarnings(ws.analysisWarnings);
                });
            });

            yield* updateUI;

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
            // If connecting different tables, create a cross-table FD
            if (params.source && params.target && params.source !== params.target) {
                Effect.runPromise(service.addCrossTableFD(params.source as TableId, params.target as TableId));
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
            Effect.runPromise(service.updateRelationPosition(node.id as TableId, node.position.x, node.position.y));
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

    // Merge handler takes IDs from the suggestion (as strings from View)
    const onMerge = useCallback((id1: string, id2: string) => {
        Effect.runPromise(service.mergeRelations(id1 as TableId, id2 as TableId));
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

// -- Helper: Map Model to React Flow --

interface ServiceRef {
    renameRelation: (id: TableId, newName: string) => Effect.Effect<void>;
    addAttribute: (id: TableId, name: string) => Effect.Effect<void>;
    deleteAttribute: (id: TableId, name: string) => Effect.Effect<void>;
    addFD: (id: TableId, lhs: string[], rhs: string[]) => Effect.Effect<void>;
    deleteFD: (id: TableId, index: number) => Effect.Effect<void>;
    deleteCrossTableFD: (index: number) => Effect.Effect<void>;
    mergeRelations: (id1: TableId, id2: TableId) => Effect.Effect<void>;
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

    // Build lookup maps
    const healthMap = new Map(workspace.health.map((h) => [h.tableId, h]));
    const idToName = new Map(workspace.relations.map(r => [r.id, r.name]));

    workspace.relations.forEach((rel: Relation) => {
        const tableHealth = healthMap.get(rel.id);

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

    // Foreign Key edges
    workspace.foreignKeys.forEach((fk, i) => {
        const fromRel = workspace.relations.find((r) => r.id === fk.fromTableId);
        const toRel = workspace.relations.find((r) => r.id === fk.toTableId);

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

    // Cross-table FD edges
    workspace.crossTableFDs.forEach((ctfd, i) => {
        const fromRel = workspace.relations.find((r) => r.id === ctfd.fromTableId);
        const toRel = workspace.relations.find((r) => r.id === ctfd.toTableId);

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
