/**
 * View.Canvas (The View)
 * The interactive visual layer built with React Flow.
 * Dumb UI, receives state via props.
 */

"use client";

import React, { useCallback, useMemo } from 'react';
import ReactFlow, {
    Node,
    Edge,
    Background,
    Controls,
    NodeTypes,
    EdgeTypes,
    Connection,
    addEdge,
    useNodesState,
    useEdgesState,
    MarkerType,
    Handle,
    Position
} from 'reactflow';
import 'reactflow/dist/style.css';
import { motion, AnimatePresence } from 'framer-motion';
import { Card, CardHeader, CardTitle, CardContent } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";

// -- Types --

export interface WorkspaceCanvasProps {
    nodes: Node[];
    edges: Edge[];
    onNodesChange: any; // Using explicit types would require importing from reactflow everywhere
    onEdgesChange: any;
    onConnect: (connection: Connection) => void;
    onNodeDragStop: (event: any, node: Node) => void;
    onGlobalOptimize: () => void;
    mergeSuggestions: readonly (readonly [string, string, string])[];
}

// -- Custom Nodes --

const TableNode = ({ data }: { data: { label: string, onNormalize?: () => void, health?: { severity: 'ok' | 'warning' | 'error', message: string } } }) => {
    return (
        <motion.div
            initial={{ scale: 0.9, opacity: 0 }}
            animate={{ scale: 1, opacity: 1 }}
            exit={{ scale: 0.9, opacity: 0 }}
            layoutId={data.label} // For magic motion layout
            className="min-w-[200px]"
        >
            <Card className={`border-2 shadow-sm bg-white/90 backdrop-blur-sm ${data.health?.severity === 'error' ? 'border-red-300' : data.health?.severity === 'warning' ? 'border-yellow-300' : 'border-slate-200'}`}>
                <CardHeader className="p-3 border-b border-slate-100 bg-slate-50/50 flex flex-row items-center justify-between space-y-0">
                    <div className="flex flex-col gap-1">
                        <CardTitle className="text-sm font-medium text-slate-700">{data.label}</CardTitle>
                        {data.health && data.health.severity !== 'ok' && (
                            <Badge variant={data.health.severity === 'error' ? 'destructive' : 'secondary'} className="text-[10px] px-1 h-5 w-fit">
                                {data.health.severity === 'error' ? 'Critical' : 'Warning'}
                            </Badge>
                        )}
                    </div>
                    <button
                        onClick={data.onNormalize}
                        className="text-xs bg-slate-200 hover:bg-slate-300 px-2 py-1 rounded text-slate-600 transition-colors"
                    >
                        Optimize
                    </button>
                </CardHeader>
                <CardContent className="p-0 min-h-[50px] bg-transparent" />
            </Card>
            {/* Invisible handle for connecting to table itself if needed */}
            <Handle type="target" position={Position.Top} className="opacity-0" />
            <Handle type="source" position={Position.Bottom} className="opacity-0" />
        </motion.div>
    );
};

const AttributeNode = ({ data }: { data: { label: string, isKey?: boolean } }) => {
    return (
        <div className="relative group px-3 py-2 bg-white border border-slate-200 rounded-md shadow-sm hover:border-slate-400 transition-colors">
            <Handle type="target" position={Position.Left} className="w-2 h-2 !bg-slate-400" />
            <div className="flex items-center gap-2">
                <span className="text-xs font-medium text-slate-600 font-mono">
                    {data.label}
                </span>
                {data.isKey && <Badge variant="secondary" className="text-[10px] h-4 px-1">PK</Badge>}
            </div>
            <Handle type="source" position={Position.Right} className="w-2 h-2 !bg-slate-400" />
        </div>
    );
};

const nodeTypes: NodeTypes = {
    table: TableNode,
    attribute: AttributeNode,
};

// -- Main Component --

// -- Canvas Overlay --

import { Button } from "@/components/ui/button";

const WorkspaceDashboard = ({ onOptimize, suggestions }: { onOptimize: () => void, suggestions: readonly (readonly [string, string, string])[] }) => (
    <div className="absolute top-4 right-4 z-10 flex flex-col gap-4 items-end pointer-events-none">
        <div className="pointer-events-auto">
            <Button onClick={onOptimize} className="shadow-lg bg-indigo-600 hover:bg-indigo-700 text-white">
                ✨ Analyze Workspace
            </Button>
        </div>

        {suggestions.length > 0 && (
            <Card className="w-80 shadow-xl pointer-events-auto bg-white/95 backdrop-blur">
                <CardHeader className="p-3 border-b">
                    <CardTitle className="text-sm font-medium flex items-center gap-2">
                        💡 Intelligent Suggestions
                    </CardTitle>
                </CardHeader>
                <CardContent className="p-3 max-h-60 overflow-y-auto">
                    <div className="flex flex-col gap-2">
                        {suggestions.map(([t1, t2, reason], i) => (
                            <div key={i} className="text-xs p-2 bg-indigo-50 border border-indigo-100 rounded text-indigo-900">
                                <span className="font-bold">{t1}</span> & <span className="font-bold">{t2}</span>
                                <p className="text-slate-600 mt-1">{reason}</p>
                            </div>
                        ))}
                    </div>
                </CardContent>
            </Card>
        )}
    </div>
);

export const WorkspaceCanvas: React.FC<WorkspaceCanvasProps> = ({
    nodes,
    edges,
    onNodesChange,
    onEdgesChange,
    onConnect,
    onNodeDragStop,
    onGlobalOptimize,
    mergeSuggestions
}) => {
    return (
        <div className="w-full h-full bg-slate-50 relative">
            <WorkspaceDashboard onOptimize={onGlobalOptimize} suggestions={mergeSuggestions} />
            <AnimatePresence>
                <ReactFlow
                    nodes={nodes}
                    edges={edges}
                    onNodesChange={onNodesChange}
                    onEdgesChange={onEdgesChange}
                    onConnect={onConnect}
                    onNodeDragStop={onNodeDragStop}
                    nodeTypes={nodeTypes}
                    fitView
                    proOptions={{ hideAttribution: true }}
                    className="bg-slate-50"
                >
                    <Background color="#cbd5e1" gap={16} />
                    <Controls className="bg-white border-slate-200 shadow-sm text-slate-600" />
                </ReactFlow>
            </AnimatePresence>
        </div>
    );
};
