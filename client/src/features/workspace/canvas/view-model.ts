import { Effect, Option, HashMap } from "effect";
import { type Node, type Edge, MarkerType } from "reactflow";
import {
  type Workspace,
  type TableId,
  type FDId,
  makeFDId,
  type CrossTableFDId,
  type SQLType,
} from "../model";

// -- View Models --

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

// -- Interfaces for Binding --

export interface ServiceBinding {
  renameRelation: (id: TableId, newName: string) => Effect.Effect<void>;
  addAttribute: (id: TableId, name: string, sqlType: SQLType) => Effect.Effect<void>;
  deleteAttribute: (id: TableId, name: string) => Effect.Effect<void>;
  addFD: (id: TableId, lhs: string[], rhs: string[]) => Effect.Effect<void>;
  deleteFD: (id: TableId, fdId: FDId) => Effect.Effect<void>;
  deleteCrossTableFD: (id: CrossTableFDId) => Effect.Effect<void>;
  mergeRelations: (id1: TableId, id2: TableId) => Effect.Effect<void>;
  deleteRelation: (id: TableId) => Effect.Effect<void>;
}

export interface OptimizerBinding {
  open: (id: string) => Effect.Effect<void>;
}

// -- Factory --

export const mapStateToFlow = (
  workspace: Workspace,
  service: ServiceBinding,
  optimizer: OptimizerBinding
): { nodes: Node[]; edges: Edge[] } => {
  const nodes: Node[] = [];
  const edges: Edge[] = [];

  // Build lookup map for health
  const healthMap = new Map<TableId, { severity: "ok" | "warning" | "error"; message: string }>();
  for (const [tableId, health] of HashMap.entries(workspace.health)) {
    healthMap.set(tableId, { severity: health.severity, message: health.message });
  }

  // Iterate over HashMap values
  for (const rel of HashMap.values(workspace.relations)) {
    const tableHealth = healthMap.get(rel.id);

    const tableData: TableData = {
      id: rel.id,
      name: rel.name,
      attributes: rel.attributes.map((a) => ({ name: a.name, sqlType: a.sqlType })),
      fds: rel.fds.map((fd) => ({
        id: fd.id,
        lhs: fd.lhs as readonly string[],
        rhs: fd.rhs as readonly string[],
      })),
      health: tableHealth,
      onRename: (newName: string) => {
        Effect.runPromise(service.renameRelation(rel.id, newName));
      },
      onAddAttribute: (name: string, sqlType: SQLType) => {
        Effect.runPromise(service.addAttribute(rel.id, name, sqlType));
      },
      onDeleteAttribute: (name: string) => {
        Effect.runPromise(service.deleteAttribute(rel.id, name));
      },
      onAddFD: (lhs: string[], rhs: string[]) => {
        Effect.runPromise(service.addFD(rel.id, lhs, rhs));
      },
      onDeleteFD: (fdId: string) => {
        Effect.runPromise(service.deleteFD(rel.id, makeFDId(fdId)));
      },
      onOptimize: () => {
        Effect.runPromise(optimizer.open(rel.id));
      },
      onDeleteRelation: () => {
        Effect.runPromise(service.deleteRelation(rel.id));
      },
    };

    nodes.push({
      id: rel.id,
      type: "table",
      position: rel.position,
      data: tableData,
    });
  }

  // Foreign Key edges
  for (const [id, fk] of HashMap.entries(workspace.foreignKeys)) {
    const fromRel = HashMap.get(workspace.relations, fk.fromTableId);
    const toRel = HashMap.get(workspace.relations, fk.toTableId);

    if (Option.isSome(fromRel) && Option.isSome(toRel)) {
      edges.push({
        id: `fk-${id}`,
        source: fromRel.value.id,
        target: toRel.value.id,
        type: "smoothstep",
        animated: false,
        style: { stroke: "#3b82f6", strokeWidth: 2 },
        markerEnd: { type: MarkerType.ArrowClosed, color: "#3b82f6" },
        label: `FK: ${fk.fromAttribute}`,
        labelStyle: { fontSize: 9, fill: "#1d4ed8", fontWeight: 600 },
        labelBgStyle: { fill: "#dbeafe", fillOpacity: 0.95 },
      });
    }
  }

  // Cross-table FD edges - use real IDs
  for (const [id, ctfd] of HashMap.entries(workspace.crossTableFDs)) {
    const fromRel = HashMap.get(workspace.relations, ctfd.fromTableId);
    const toRel = HashMap.get(workspace.relations, ctfd.toTableId);

    if (Option.isSome(fromRel) && Option.isSome(toRel)) {
      edges.push({
        id: `cross-${id}`,
        source: fromRel.value.id,
        target: toRel.value.id,
        animated: true,
        style: { stroke: "#f59e0b", strokeWidth: 2, strokeDasharray: "5,5" },
        markerEnd: { type: MarkerType.ArrowClosed, color: "#f59e0b" },
        label: ctfd.suggestion,
        labelStyle: { fontSize: 9, fill: "#92400e" },
        labelBgStyle: { fill: "#fef3c7", fillOpacity: 0.9 },
      });
    }
  }

  return { nodes, edges };
};
