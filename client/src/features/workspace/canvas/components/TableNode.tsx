import React, { useState, useCallback } from "react";
import { Handle, Position } from "reactflow";
import { motion } from "framer-motion";
import { Card, CardHeader, CardTitle, CardContent } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
  DialogTrigger,
  DialogClose,
} from "@/components/ui/dialog";
import { RelationshipBuilder } from "./RelationshipBuilder";
import { SQL_TYPES, type SQLType } from "../../model";

export interface TableData {
  id: string;
  name: string;
  attributes: readonly { name: string; sqlType: string }[];
  fds: readonly { id: string; lhs: readonly string[]; rhs: readonly string[] }[];
  candidateKeys?: readonly (readonly string[])[];
  health?: { severity: "ok" | "warning" | "error"; message: string };
  isLoading?: boolean;
  onRename: (newName: string) => void;
  onAddAttribute: (name: string, sqlType: SQLType) => void;
  onDeleteAttribute: (name: string) => void;
  onAddFD: (lhs: string[], rhs: string[]) => void;
  onDeleteFD: (fdId: string) => void;
  onOptimize: () => void;
  onDeleteRelation: () => void;
}

export const TableNode = ({ data }: { data: TableData }): React.ReactElement => {
  const [isAddingAttr, setIsAddingAttr] = useState(false);
  const [newAttrName, setNewAttrName] = useState("");
  const [newAttrType, setNewAttrType] = useState<SQLType>("TEXT");
  const [isAddingFD, setIsAddingFD] = useState(false);

  const [isEditingName, setIsEditingName] = useState(false);
  const [editName, setEditName] = useState(data.name);

  const handleAddAttr = useCallback((): void => {
    if (newAttrName.trim()) {
      data.onAddAttribute(newAttrName.trim(), newAttrType);
      setNewAttrName("");
      setNewAttrType("TEXT");
      setIsAddingAttr(false);
    }
  }, [newAttrName, newAttrType, data]);

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

              <Dialog>
                <DialogTrigger asChild>
                  <button
                    className="text-[10px] bg-red-100 hover:bg-red-200 px-1.5 py-0.5 rounded text-red-700 font-medium transition-colors"
                    title="Delete Table"
                  >
                    🗑️
                  </button>
                </DialogTrigger>
                <DialogContent className="sm:max-w-[425px]">
                  <DialogHeader>
                    <DialogTitle>Delete Table</DialogTitle>
                    <DialogDescription>
                      Are you sure you want to delete table{" "}
                      <b className="text-slate-900">{data.name}</b>? This action cannot be undone
                      and will remove all associated relationships.
                    </DialogDescription>
                  </DialogHeader>
                  <DialogFooter>
                    <DialogClose asChild>
                      <button className="text-sm px-3 py-2 rounded hover:bg-slate-100 transition-colors text-slate-600">
                        Cancel
                      </button>
                    </DialogClose>
                    <DialogClose asChild>
                      <button
                        onClick={data.onDeleteRelation}
                        className="text-sm px-3 py-2 bg-red-600 hover:bg-red-700 text-white rounded transition-colors font-medium"
                      >
                        Delete
                      </button>
                    </DialogClose>
                  </DialogFooter>
                </DialogContent>
              </Dialog>
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
                key={`${attr.name}-${idx}`}
                className="flex items-center justify-between group px-2 py-1.5 bg-slate-50 rounded hover:bg-slate-100 transition-colors"
              >
                <div className="flex items-center gap-2">
                  <span className="text-xs font-mono text-slate-700">{attr.name}</span>
                  <span className="text-[9px] text-slate-400 bg-slate-200 px-1 rounded">
                    {attr.sqlType}
                  </span>
                </div>
                <button
                  onClick={() => data.onDeleteAttribute(attr.name)}
                  className="w-5 h-5 flex items-center justify-center text-slate-400 hover:text-red-500 hover:bg-red-50 rounded transition-colors"
                  title="Delete attribute"
                >
                  <svg
                    xmlns="http://www.w3.org/2000/svg"
                    width="12"
                    height="12"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="currentColor"
                    strokeWidth="2"
                    strokeLinecap="round"
                    strokeLinejoin="round"
                  >
                    <path d="M3 6h18" />
                    <path d="M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6" />
                    <path d="M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2" />
                  </svg>
                </button>
              </div>
            ))}
            {data.attributes.length === 0 && !isAddingAttr && (
              <div className="text-[10px] text-slate-400 italic px-2 py-1">No attributes yet</div>
            )}
            {isAddingAttr && (
              <div className="flex flex-col gap-2 mt-2 p-2 bg-slate-100 rounded">
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
                      setNewAttrType("TEXT");
                      setIsAddingAttr(false);
                    }
                  }}
                  placeholder="attribute_name"
                  className="text-xs font-mono w-full px-2 py-1 border border-indigo-300 rounded focus:outline-none focus:ring-1 focus:ring-indigo-400"
                  autoFocus
                />
                <select
                  value={newAttrType}
                  onChange={(e) => setNewAttrType(e.target.value as SQLType)}
                  className="text-xs w-full px-2 py-1 border border-slate-300 rounded bg-white focus:outline-none focus:ring-1 focus:ring-indigo-400"
                >
                  {SQL_TYPES.map((t) => (
                    <option key={t} value={t}>
                      {t}
                    </option>
                  ))}
                </select>
                <div className="flex gap-1">
                  <button
                    onClick={handleAddAttr}
                    className="flex-1 text-[10px] bg-indigo-500 text-white px-2 py-1 rounded hover:bg-indigo-600"
                  >
                    Add
                  </button>
                  <button
                    onClick={() => {
                      setNewAttrName("");
                      setNewAttrType("TEXT");
                      setIsAddingAttr(false);
                    }}
                    className="text-[10px] text-slate-500 px-2 py-1"
                  >
                    Cancel
                  </button>
                </div>
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
                {data.fds.map((fd) => (
                  <div
                    key={fd.id}
                    className="flex items-center justify-between group px-2 py-1.5 bg-amber-50 rounded hover:bg-amber-100 transition-colors"
                  >
                    <span className="text-xs font-mono text-amber-800">{formatFD(fd)}</span>
                    <button
                      onClick={() => data.onDeleteFD(fd.id)}
                      className="w-5 h-5 flex items-center justify-center text-amber-400 hover:text-red-500 hover:bg-red-50 rounded transition-colors"
                      title="Delete FD"
                    >
                      <svg
                        xmlns="http://www.w3.org/2000/svg"
                        width="12"
                        height="12"
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="currentColor"
                        strokeWidth="2"
                        strokeLinecap="round"
                        strokeLinejoin="round"
                      >
                        <path d="M3 6h18" />
                        <path d="M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6" />
                        <path d="M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2" />
                      </svg>
                    </button>
                  </div>
                ))}
              </div>
            </>
          )}

          {isAddingFD && (
            <div className="mt-2">
              <RelationshipBuilder
                attributes={data.attributes.map((a) => a.name)}
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
            (data.health.severity !== "ok" ||
              data.health.message.includes("BCNF") ||
              data.health.message.includes("3NF")) && (
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
