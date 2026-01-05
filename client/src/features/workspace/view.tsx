/**
 * View.Canvas (The View)
 * The interactive visual layer built with React Flow.
 * Dumb UI, receives state via props.
 */

"use client";

import React, { useState, useCallback, useEffect } from "react";
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
import { ArrowDown } from "lucide-react";

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

// Merge suggestion with resolved display names
export interface MergeSuggestionDisplay {
  id1: string;
  id2: string;
  name1: string;
  name2: string;
  reason: string;
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
  onExportSQL: () => void;
  isAnalyzing?: boolean;
  mergeSuggestions: readonly MergeSuggestionDisplay[];
  analysisWarnings: readonly string[];
  onMergeRelations: (id1: string, id2: string) => void;
}
// -- Relationship Builder Component --
const RelationshipBuilder = ({
  attributes,
  onSave,
  onCancel,
}: {
  attributes: readonly string[];
  onSave: (lhs: string[], rhs: string[]) => void;
  onCancel: () => void;
}) => {
  const [lhs, setLhs] = useState<Set<string>>(new Set());
  const [rhs, setRhs] = useState<Set<string>>(new Set());

  const toggleLhs = (attr: string) => {
    const next = new Set(lhs);
    if (next.has(attr)) {
      next.delete(attr);
    } else {
      next.add(attr);
    }
    setLhs(next);
  };

  const toggleRhs = (attr: string) => {
    const next = new Set(rhs);
    if (next.has(attr)) {
      next.delete(attr);
    } else {
      next.add(attr);
    }
    setRhs(next);
  };

  const handleSave = () => {
    if (lhs.size > 0 && rhs.size > 0) {
      onSave(Array.from(lhs), Array.from(rhs));
    }
  };

  return (
    <div className="p-3 bg-slate-50 rounded-md border border-slate-200 space-y-3">
      <div className="text-xs font-semibold text-slate-700">Define Dependency</div>

      <div className="space-y-1">
        <div className="text-[10px] uppercase text-slate-500 font-bold tracking-wider">
          Determinants
        </div>
        <div className="flex flex-wrap gap-1">
          {attributes.map((attr) => (
            <Badge
              key={attr}
              variant={lhs.has(attr) ? "default" : "outline"}
              className="cursor-pointer hover:bg-slate-200 data-[state=active]:hover:bg-primary/90"
              data-state={lhs.has(attr) ? "active" : "inactive"}
              onClick={() => toggleLhs(attr)}
            >
              {attr}
            </Badge>
          ))}
        </div>
      </div>

      <div className="flex justify-center text-slate-400">
        <ArrowDown className="w-4 h-4" />
      </div>

      <div className="space-y-1">
        <div className="text-[10px] uppercase text-slate-500 font-bold tracking-wider">
          Dependents
        </div>
        <div className="flex flex-wrap gap-1">
          {attributes.map((attr) => (
            <Badge
              key={attr}
              variant="outline"
              className={`cursor-pointer hover:bg-slate-200 ${
                rhs.has(attr)
                  ? "bg-amber-100 text-amber-800 border-amber-200 hover:bg-amber-200"
                  : ""
              }`}
              onClick={() => toggleRhs(attr)}
            >
              {attr}
            </Badge>
          ))}
        </div>
      </div>

      <div className="flex justify-end gap-2 pt-2 border-t border-slate-200">
        <Button size="sm" variant="ghost" onClick={onCancel} className="h-6 px-2 text-xs">
          Cancel
        </Button>
        <Button
          size="sm"
          onClick={handleSave}
          disabled={lhs.size === 0 || rhs.size === 0}
          className="h-6 px-2 text-xs bg-indigo-600 hover:bg-indigo-700 text-white"
        >
          Add Rule
        </Button>
      </div>
    </div>
  );
};

// -- Custom Nodes --

const TableNode = ({ data }: { data: TableData }): React.ReactElement => {
  const [isAddingAttr, setIsAddingAttr] = useState(false);
  const [newAttrName, setNewAttrName] = useState("");
  const [isAddingFD, setIsAddingFD] = useState(false);

  const [isEditingName, setIsEditingName] = useState(false);
  const [editName, setEditName] = useState(data.name);

  const handleAddAttr = useCallback((): void => {
    if (newAttrName.trim()) {
      data.onAddAttribute(newAttrName.trim());
      setNewAttrName("");
      setIsAddingAttr(false);
    }
  }, [newAttrName, data]);

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
              ? data.health?.message.includes("3NF")
                ? "border-blue-400" // 3NF is acceptable/info
                : "border-amber-400" // True warning
              : "border-emerald-400" // BCNF/Healthy
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
                  className={`text-[9px] px-1.5 h-4 shrink-0 ${data.health.message?.includes("3NF") ? "bg-blue-100 text-blue-800 hover:bg-blue-200" : ""}`}
                >
                  {data.health.severity === "error"
                    ? "⚠ Critical"
                    : data.health.message?.includes("3NF")
                      ? "ℹ 3NF"
                      : "⚡ Warning"}
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
                disabled={data.attributes.length === 0}
                className="text-[10px] bg-slate-100 hover:bg-slate-200 px-1.5 py-0.5 rounded text-slate-700 font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
                title={data.attributes.length === 0 ? "Add attributes first" : "Add Rule"}
              >
                +Rule
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
            <div className="mt-2">
              <RelationshipBuilder
                attributes={data.attributes}
                onSave={(lhs, rhs) => {
                  data.onAddFD(lhs, rhs);
                  setIsAddingFD(false);
                }}
                onCancel={() => setIsAddingFD(false)}
              />
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
          {data.health &&
            (data.health.severity !== "ok" || data.health.message.includes("BCNF")) && (
              <div
                className={`mt-3 p-2 rounded text-xs ${
                  data.health.severity === "error"
                    ? "bg-red-50 border border-red-200 text-red-700"
                    : data.health.severity === "warning"
                      ? data.health.message.includes("3NF")
                        ? "bg-blue-50 border border-blue-200 text-blue-700"
                        : "bg-amber-50 border border-amber-200 text-amber-700"
                      : "bg-emerald-50 border border-emerald-200 text-emerald-700"
                }`}
              >
                <span className="font-medium">
                  {data.health.severity === "error"
                    ? "⚠️ "
                    : data.health.severity === "warning"
                      ? data.health.message.includes("3NF")
                        ? "ℹ️ "
                        : "⚡ "
                      : "✅ "}
                </span>
                {data.health.message}
                {data.health.severity !== "ok" && !data.health.message.includes("BCNF") && (
                  <div
                    className="mt-1 font-semibold hover:underline cursor-pointer"
                    onClick={data.onOptimize}
                  >
                    👉 Click ⚙ to optimize
                  </div>
                )}
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

const SuggestionsCard = ({
  suggestions,
  onClose,
  onApply,
}: {
  suggestions: readonly MergeSuggestionDisplay[];
  onClose: () => void;
  onApply: (id1: string, id2: string) => void;
}) => (
  <Card className="w-80 shadow-xl pointer-events-auto bg-white/95 backdrop-blur border-indigo-100">
    <CardHeader className="p-3 border-b border-indigo-50 bg-gradient-to-r from-indigo-50 to-purple-50 flex flex-row items-center justify-between space-y-0">
      <CardTitle className="text-sm font-medium flex items-center gap-2 text-indigo-900">
        💡 Intelligent Suggestions
      </CardTitle>
      <button onClick={onClose} className="text-indigo-400 hover:text-indigo-600 transition-colors">
        ×
      </button>
    </CardHeader>
    <CardContent className="p-3 max-h-60 overflow-y-auto">
      <div className="flex flex-col gap-2">
        {suggestions.map((s, i) => (
          <div
            key={i}
            className="text-xs p-2 bg-indigo-50 border border-indigo-100 rounded text-indigo-900 flex justify-between items-center"
          >
            <div>
              <span className="font-bold">{s.name1}</span> &{" "}
              <span className="font-bold">{s.name2}</span>
              <p className="text-slate-600 mt-1">{s.reason}</p>
            </div>
            <Button
              size="sm"
              className="h-6 px-2 text-[10px] bg-indigo-200 text-indigo-800 hover:bg-indigo-300 ml-2 shadow-none border border-indigo-300"
              onClick={() => onApply(s.id1, s.id2)}
            >
              Merge
            </Button>
          </div>
        ))}
      </div>
    </CardContent>
  </Card>
);

// -- Dashboard Overlay --

const WorkspaceDashboardImpl = ({
  onOptimize,
  onAddTable,
  onExportSQL,
  suggestions,
  isAnalyzing,
  analysisWarnings,
  onMerge,
}: {
  onOptimize: () => void;
  onAddTable: () => void;
  onExportSQL: () => void;
  suggestions: readonly MergeSuggestionDisplay[];
  isAnalyzing?: boolean;
  analysisWarnings: readonly string[];
  onMerge: (id1: string, id2: string) => void;
}): React.ReactElement => {
  const [showSuggestions, setShowSuggestions] = useState(true);

  // Reset showSuggestions when new suggestions arrive
  useEffect(() => {
    if (suggestions.length > 0) {
      setShowSuggestions(true);
    }
  }, [suggestions]);

  return (
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
          variant="outline"
          className="shadow-lg bg-white/90 backdrop-blur hover:bg-slate-50 text-slate-700 border-slate-200"
          onClick={onExportSQL}
        >
          💾 SQL
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

      {showSuggestions && suggestions.length > 0 && (
        <SuggestionsCard
          suggestions={suggestions}
          onClose={() => setShowSuggestions(false)}
          onApply={onMerge}
        />
      )}

      {analysisWarnings.length > 0 && (
        <Card className="w-80 shadow-xl pointer-events-auto bg-white/95 backdrop-blur border-amber-100">
          <CardHeader className="p-3 border-b border-amber-50 bg-gradient-to-r from-amber-50 to-orange-50">
            <CardTitle className="text-sm font-medium flex items-center gap-2 text-amber-900">
              ⚠️ Analysis Warnings
            </CardTitle>
          </CardHeader>
          <CardContent className="p-3 max-h-60 overflow-y-auto">
            <div className="flex flex-col gap-2">
              {analysisWarnings.map((w, i) => (
                <div
                  key={i}
                  className="text-xs p-2 bg-amber-50 border border-amber-100 rounded text-amber-900"
                >
                  <p>{w}</p>
                </div>
              ))}
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  );
};

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
  onExportSQL,
  isAnalyzing,
  mergeSuggestions,
  analysisWarnings,
  onMergeRelations,
}): React.ReactElement => {
  return (
    <div className="w-full h-full bg-gradient-to-br from-slate-50 to-slate-100 relative">
      <WorkspaceDashboardImpl
        onOptimize={onGlobalOptimize}
        onAddTable={onAddTable}
        onExportSQL={onExportSQL}
        suggestions={mergeSuggestions}
        isAnalyzing={isAnalyzing}
        analysisWarnings={analysisWarnings}
        onMerge={onMergeRelations}
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
