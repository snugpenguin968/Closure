import React, { useState, useCallback } from "react";
import { Handle, Position } from "reactflow";
import { motion } from "framer-motion";
import { Card, CardHeader, CardTitle, CardContent } from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { type TableData } from "../view-model";
import { RelationshipBuilder } from "./RelationshipBuilder";

export const TableNode = ({ data }: { data: TableData }): React.ReactElement => {
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
                className={`border-2 shadow-lg bg-white/95 backdrop-blur-sm ${data.health?.severity === "error"
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
                                {data.fds.map((fd) => (
                                    <div
                                        key={fd.id}
                                        className="flex items-center justify-between group px-2 py-1 bg-amber-50 rounded hover:bg-amber-100 transition-colors"
                                    >
                                        <span className="text-xs font-mono text-amber-800">{formatFD(fd)}</span>
                                        <button
                                            onClick={() => data.onDeleteFD(fd.id)}
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
                        (data.health.severity !== "ok" || data.health.message.includes("BCNF") || data.health.message.includes("3NF")) && (
                            <div
                                className={`mt-3 p-2 rounded text-xs ${data.health.severity === "error"
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
