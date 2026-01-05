/**
 * View.Canvas (The View)
 * The interactive visual layer built with React Flow.
 * Dumb UI, receives state via props.
 */

"use client";

import React, { useState, useCallback } from "react";
import ReactFlow, {
  type Node,
  type Edge,
  type NodeChange,
  type EdgeChange,
  Background,
  Controls,
  type NodeTypes,
  type Connection,
  Handle,
  Position,
} from "reactflow";
import "reactflow/dist/style.css";
import { motion } from "framer-motion";
import { Card, CardHeader, CardTitle, CardContent } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";

// -- Types --

export interface TableData {
  id: string;
  name: string;
  attributes: readonly string[];
  fds: readonly { lhs: readonly string[]; rhs: readonly string[] }[];
  candidateKeys?: readonly (readonly string[])[]; // Candidate keys from analysis
  health?: { severity: "ok" | "warning" | "error"; message: string };
  isLoading?: boolean; // Loading state for optimization
  // Callbacks
  onRename: (newName: string) => void;
  onAddAttribute: (name: string) => void;
  onDeleteAttribute: (name: string) => void;
  onAddFD: (lhs: string[], rhs: string[]) => void;
  onDeleteFD: (index: number) => void;
  onOptimize: () => void;
}

export interface WorkspaceCanvasProps {
  nodes: Node[];
  edges: Edge[];
  onNodesChange: (changes: NodeChange[]) => void;
  onEdgesChange: (changes: EdgeChange[]) => void;
  onConnect: (connection: Connection) => void;
  onNodeDragStop: (event: React.MouseEvent, node: Node) => void;
  onEdgeClick?: (event: React.MouseEvent, edge: Edge) => void;
  onGlobalOptimize: () => void;
  onAddTable: () => void;
  isAnalyzing?: boolean;
  mergeSuggestions: readonly (readonly [string, string, string])[];
}

// -- Custom Nodes --

const TableNode = ({ data }: { data: TableData }): React.ReactElement => {
  const [isAddingAttr, setIsAddingAttr] = useState(false);
  const [newAttrName, setNewAttrName] = useState("");
  const [isAddingFD, setIsAddingFD] = useState(false);
  const [fdLhs, setFdLhs] = useState("");
  const [fdRhs, setFdRhs] = useState("");
  const [isEditingName, setIsEditingName] = useState(false);
  const [editName, setEditName] = useState(data.name);

  const handleAddAttr = useCallback((): void => {
    if (newAttrName.trim()) {
      data.onAddAttribute(newAttrName.trim());
      setNewAttrName("");
      setIsAddingAttr(false);
    }
  }, [newAttrName, data]);

  const handleAddFD = useCallback((): void => {
    const lhs = fdLhs
      .split(",")
      .map((s) => s.trim())
      .filter(Boolean);
    const rhs = fdRhs
      .split(",")
      .map((s) => s.trim())
      .filter(Boolean);
    if (lhs.length > 0 && rhs.length > 0) {
      data.onAddFD(lhs, rhs);
      setFdLhs("");
      setFdRhs("");
      setIsAddingFD(false);
    }
  }, [fdLhs, fdRhs, data]);

  const handleRename = useCallback((): void => {
    if (editName.trim() && editName !== data.name) {
      data.onRename(editName.trim());
    }
    setIsEditingName(false);
  }, [editName, data]);

  const formatFD = (fd: { lhs: readonly string[]; rhs: readonly string[] }): string => {
    return `${fd.lhs.join(", ")} → ${fd.rhs.join(", ")}`;
  };

  return (
    <motion.div
      initial={{ scale: 0.9, opacity: 0 }}
      animate={{ scale: 1, opacity: 1 }}
      exit={{ scale: 0.9, opacity: 0 }}
      className="min-w-[280px] max-w-[350px]"
    >
      <Card
        className={`border-2 shadow-lg bg-white/95 backdrop-blur-sm ${
          data.health?.severity === "error"
            ? "border-red-400"
            : data.health?.severity === "warning"
              ? "border-amber-400"
              : "border-slate-200"
        }`}
      >
        {/* Header */}
        <CardHeader className="p-3 border-b border-slate-100 bg-gradient-to-r from-slate-50 to-slate-100/50">
          <div className="flex items-center justify-between gap-2">
            <div className="flex items-center gap-2 flex-1 min-w-0">
              {isEditingName ? (
                <input
                  type="text"
                  value={editName}
                  onChange={(e) => setEditName(e.target.value)}
                  onBlur={handleRename}
                  onKeyDown={(e) => {
                    if (e.key === "Enter") {
                      handleRename();
                    }
                    if (e.key === "Escape") {
                      setEditName(data.name);
                      setIsEditingName(false);
                    }
                  }}
                  className="text-sm font-semibold text-slate-800 bg-white border border-indigo-300 rounded px-2 py-0.5 w-full focus:outline-none focus:ring-2 focus:ring-indigo-400"
                  autoFocus
                />
              ) : (
                <CardTitle
                  className="text-sm font-semibold text-slate-800 cursor-pointer hover:text-indigo-600 truncate"
                  onClick={() => setIsEditingName(true)}
                  title="Click to rename"
                >
                  {data.name}
                </CardTitle>
              )}
              {data.health && data.health.severity !== "ok" && (
                <Badge
                  variant={data.health.severity === "error" ? "destructive" : "secondary"}
                  className="text-[9px] px-1.5 h-4 shrink-0"
                >
                  {data.health.severity === "error" ? "⚠ Critical" : "⚡ Warning"}
                </Badge>
              )}
            </div>
            <div className="flex gap-1 shrink-0">
              <button
                onClick={() => setIsAddingAttr(true)}
                className="text-[10px] bg-indigo-100 hover:bg-indigo-200 px-1.5 py-0.5 rounded text-indigo-700 font-medium transition-colors"
                title="Add Attribute"
              >
                +A
              </button>
              <button
                onClick={() => setIsAddingFD(true)}
                className="text-[10px] bg-slate-100 hover:bg-slate-200 px-1.5 py-0.5 rounded text-slate-700 font-medium transition-colors"
                title="Add FD"
              >
                +FD
              </button>
              <button
                onClick={data.onOptimize}
                className="text-[10px] bg-emerald-100 hover:bg-emerald-200 px-1.5 py-0.5 rounded text-emerald-700 font-medium transition-colors"
                title="Optimize"
              >
                ⚙
              </button>
            </div>
          </div>
        </CardHeader>

        {/* Attributes */}
        <CardContent className="p-2">
          <div className="text-[10px] font-medium text-slate-500 uppercase tracking-wide mb-1">
            Attributes
          </div>
          <div className="flex flex-col gap-0.5">
            {data.attributes.map((attr, idx) => (
              <div
                key={`${attr}-${idx}`}
                className="flex items-center justify-between group px-2 py-1 bg-slate-50 rounded hover:bg-slate-100 transition-colors"
              >
                <span className="text-xs font-mono text-slate-700">{attr}</span>
                <button
                  onClick={() => data.onDeleteAttribute(attr)}
                  className="text-[10px] text-slate-400 hover:text-red-500 opacity-0 group-hover:opacity-100 transition-opacity"
                  title="Delete"
                >
                  ×
                </button>
              </div>
            ))}
            {data.attributes.length === 0 && !isAddingAttr && (
              <div className="text-[10px] text-slate-400 italic px-2 py-1">No attributes yet</div>
            )}
            {isAddingAttr && (
              <div className="flex items-center gap-1 mt-1">
                <input
                  type="text"
                  value={newAttrName}
                  onChange={(e) => setNewAttrName(e.target.value)}
                  onKeyDown={(e) => {
                    if (e.key === "Enter") {
                      handleAddAttr();
                    }
                    if (e.key === "Escape") {
                      setNewAttrName("");
                      setIsAddingAttr(false);
                    }
                  }}
                  placeholder="attribute_name"
                  className="text-xs font-mono flex-1 px-2 py-1 border border-indigo-300 rounded focus:outline-none focus:ring-1 focus:ring-indigo-400"
                  autoFocus
                />
                <button
                  onClick={handleAddAttr}
                  className="text-[10px] bg-indigo-500 text-white px-2 py-1 rounded hover:bg-indigo-600"
                >
                  Add
                </button>
                <button
                  onClick={() => {
                    setNewAttrName("");
                    setIsAddingAttr(false);
                  }}
                  className="text-[10px] text-slate-500 px-1"
                >
                  ×
                </button>
              </div>
            )}
          </div>

          {/* FDs */}
          {(data.fds.length > 0 || isAddingFD) && (
            <>
              <div className="text-[10px] font-medium text-slate-500 uppercase tracking-wide mt-3 mb-1">
                Functional Dependencies
              </div>
              <div className="flex flex-col gap-0.5">
                {data.fds.map((fd, idx) => (
                  <div
                    key={idx}
                    className="flex items-center justify-between group px-2 py-1 bg-amber-50 rounded hover:bg-amber-100 transition-colors"
                  >
                    <span className="text-xs font-mono text-amber-800">{formatFD(fd)}</span>
                    <button
                      onClick={() => data.onDeleteFD(idx)}
                      className="text-[10px] text-amber-400 hover:text-red-500 opacity-0 group-hover:opacity-100 transition-opacity"
                      title="Delete"
                    >
                      ×
                    </button>
                  </div>
                ))}
              </div>
            </>
          )}

          {isAddingFD && (
            <div className="mt-2 p-2 bg-slate-50 rounded border border-slate-200">
              <div className="text-[10px] text-slate-600 mb-1">New FD</div>
              <div className="flex flex-col gap-1">
                <input
                  type="text"
                  value={fdLhs}
                  onChange={(e) => setFdLhs(e.target.value)}
                  placeholder="LHS (comma separated)"
                  className="text-xs font-mono px-2 py-1 border border-slate-300 rounded focus:outline-none focus:ring-1 focus:ring-indigo-400"
                  autoFocus
                />
                <div className="text-center text-slate-400 text-xs">→</div>
                <input
                  type="text"
                  value={fdRhs}
                  onChange={(e) => setFdRhs(e.target.value)}
                  placeholder="RHS (comma separated)"
                  className="text-xs font-mono px-2 py-1 border border-slate-300 rounded focus:outline-none focus:ring-1 focus:ring-indigo-400"
                  onKeyDown={(e) => {
                    if (e.key === "Enter") {
                      handleAddFD();
                    }
                    if (e.key === "Escape") {
                      setFdLhs("");
                      setFdRhs("");
                      setIsAddingFD(false);
                    }
                  }}
                />
                <div className="flex gap-1 mt-1">
                  <button
                    onClick={handleAddFD}
                    className="text-[10px] bg-indigo-500 text-white px-2 py-1 rounded hover:bg-indigo-600 flex-1"
                  >
                    Add FD
                  </button>
                  <button
                    onClick={() => {
                      setFdLhs("");
                      setFdRhs("");
                      setIsAddingFD(false);
                    }}
                    className="text-[10px] text-slate-500 px-2 py-1"
                  >
                    Cancel
                  </button>
                </div>
              </div>
            </div>
          )}

          {/* Candidate Keys */}
          {data.candidateKeys && data.candidateKeys.length > 0 && (
            <>
              <div className="text-[10px] font-medium text-slate-500 uppercase tracking-wide mt-3 mb-1">
                Candidate Keys
              </div>
              <div className="flex flex-wrap gap-1">
                {data.candidateKeys.map((key, idx) => (
                  <Badge
                    key={idx}
                    variant="outline"
                    className="text-[9px] font-mono bg-emerald-50 text-emerald-700 border-emerald-200"
                  >
                    🔑 {key.join(", ")}
                  </Badge>
                ))}
              </div>
            </>
          )}

          {/* Health Message */}
          {data.health && data.health.severity !== "ok" && (
            <div
              className={`mt-3 p-2 rounded text-xs ${
                data.health.severity === "error"
                  ? "bg-red-50 border border-red-200 text-red-700"
                  : "bg-amber-50 border border-amber-200 text-amber-700"
              }`}
            >
              <span className="font-medium">
                {data.health.severity === "error" ? "⚠️ " : "⚡ "}
              </span>
              {data.health.message}
            </div>
          )}
        </CardContent>

        {/* Loading Overlay */}
        {data.isLoading && (
          <div className="absolute inset-0 bg-white/80 backdrop-blur-sm rounded-lg flex items-center justify-center z-10">
            <div className="flex flex-col items-center gap-2">
              <div className="w-6 h-6 border-2 border-indigo-600 border-t-transparent rounded-full animate-spin" />
              <span className="text-xs text-indigo-600 font-medium">Analyzing...</span>
            </div>
          </div>
        )}
      </Card>
      <Handle
        type="target"
        position={Position.Top}
        className="!w-3 !h-3 !bg-indigo-400 !border-2 !border-white"
      />
      <Handle
        type="source"
        position={Position.Bottom}
        className="!w-3 !h-3 !bg-indigo-400 !border-2 !border-white"
      />
    </motion.div>
  );
};

const nodeTypes: NodeTypes = {
  table: TableNode,
};

// -- Dashboard Overlay --

const WorkspaceDashboard = ({
  onOptimize,
  onAddTable,
  suggestions,
  isAnalyzing,
}: {
  onOptimize: () => void;
  onAddTable: () => void;
  suggestions: readonly (readonly [string, string, string])[];
  isAnalyzing?: boolean;
}): React.ReactElement => (
  <div className="absolute top-4 right-4 z-10 flex flex-col gap-4 items-end pointer-events-none">
    <div className="pointer-events-auto flex gap-2">
      <Button
        onClick={onAddTable}
        variant="secondary"
        className="shadow-lg bg-white hover:bg-slate-100 text-slate-700 border border-slate-200"
      >
        + New Table
      </Button>
      <Button
        onClick={onOptimize}
        disabled={isAnalyzing}
        className="shadow-lg bg-gradient-to-r from-indigo-600 to-purple-600 hover:from-indigo-700 hover:to-purple-700 text-white disabled:opacity-70"
      >
        {isAnalyzing ? (
          <>
            <span className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin mr-2" />
            Analyzing...
          </>
        ) : (
          "✨ Analyze Workspace"
        )}
      </Button>
    </div>

    {suggestions.length > 0 && (
      <Card className="w-80 shadow-xl pointer-events-auto bg-white/95 backdrop-blur border-indigo-100">
        <CardHeader className="p-3 border-b border-indigo-50 bg-gradient-to-r from-indigo-50 to-purple-50">
          <CardTitle className="text-sm font-medium flex items-center gap-2 text-indigo-900">
            💡 Intelligent Suggestions
          </CardTitle>
        </CardHeader>
        <CardContent className="p-3 max-h-60 overflow-y-auto">
          <div className="flex flex-col gap-2">
            {suggestions.map(([t1, t2, reason], i) => (
              <div
                key={i}
                className="text-xs p-2 bg-indigo-50 border border-indigo-100 rounded text-indigo-900"
              >
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

// -- Main Component --

export const WorkspaceCanvas: React.FC<WorkspaceCanvasProps> = ({
  nodes,
  edges,
  onNodesChange,
  onEdgesChange,
  onConnect,
  onNodeDragStop,
  onEdgeClick,
  onGlobalOptimize,
  onAddTable,
  isAnalyzing,
  mergeSuggestions,
}): React.ReactElement => {
  return (
    <div className="w-full h-full bg-gradient-to-br from-slate-50 to-slate-100 relative">
      <WorkspaceDashboard
        onOptimize={onGlobalOptimize}
        onAddTable={onAddTable}
        suggestions={mergeSuggestions}
        isAnalyzing={isAnalyzing}
      />
      <ReactFlow
        nodes={nodes}
        edges={edges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onConnect={onConnect}
        onNodeDragStop={onNodeDragStop}
        onEdgeClick={onEdgeClick}
        nodeTypes={nodeTypes}
        defaultViewport={{ x: 100, y: 100, zoom: 1 }}
        minZoom={0.1}
        maxZoom={2}
        deleteKeyCode={["Backspace", "Delete"]}
        proOptions={{ hideAttribution: true }}
        className="bg-transparent"
      >
        <Background color="#cbd5e1" gap={20} size={1} />
        <Controls className="bg-white border-slate-200 shadow-lg rounded-lg" />
      </ReactFlow>
    </div>
  );
};
