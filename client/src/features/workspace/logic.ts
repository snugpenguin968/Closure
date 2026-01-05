/**
 * Domain Logic Layer
 * Pure functions for domain transformations and mapping.
 * No side effects.
 *
 * Design:
 * - Structural typing for inputs/outputs.
 * - Separates "how to calculate" from "when to update".
 */

import { HashMap } from "effect";
import {
  type Relation,
  type TableId,
  type Position,
  type Attribute,
  type FDId,
  type BackendRelation,
  type Workspace,
  type TableHealth,
  type MergeSuggestion,
  type BackendTreeNode,
  type BackendHealth,
  type BackendWorkspaceResponse,
} from "./model";
import { type LayoutNode } from "./layout";

// -- Types --

export interface RelationMapper {
  toRelation: (br: BackendRelation, pos: Position) => Relation;
}

export interface TreeMapper {
  backendTreeToLayoutNode: (tree: BackendTreeNode) => LayoutNode;
}

// -- Implementation --

const asAttribute = (s: string): Attribute => s as Attribute;

const generateFDId = (): FDId => crypto.randomUUID() as FDId;

export const toRelation = (br: BackendRelation, position: Position = { x: 0, y: 0 }): Relation => ({
  id: crypto.randomUUID() as TableId,
  name: br.rjName,
  attributes: br.rjAttributes.map((s) => asAttribute(s)),
  fds: br.rjFDs.map((fd) => ({
    id: generateFDId(),
    lhs: fd.fjLhs.map((s) => asAttribute(s)),
    rhs: fd.fjRhs.map((s) => asAttribute(s)),
  })),
  position,
});

export const backendTreeToLayoutNode = (tree: BackendTreeNode): LayoutNode => ({
  name: tree.tnRelation.rjName,
  children: tree.tnChildren.map(backendTreeToLayoutNode),
});

export const buildNameToIdMap = (ws: Workspace): Map<string, TableId> => {
  const result = new Map<string, TableId>();
  for (const [id, rel] of HashMap.entries(ws.relations)) {
    result.set(rel.name, id);
  }
  return result;
};

// -- Response Mapping Helpers --

type BackendTableHealthList = typeof BackendWorkspaceResponse.Type.wresHealth;

export const mapBackendHealth = (
  backendHealth: BackendTableHealthList,
  nameToId: Map<string, TableId>,
  strategy?: "bcnf" | "3nf" | "performance"
): HashMap.HashMap<TableId, TableHealth> => {
  let newHealth = HashMap.empty<TableId, TableHealth>();
  for (const h of backendHealth) {
    const tableId = nameToId.get(h.thjTableName);
    if (tableId) {
      let message = h.thjMessage;
      // Contextualize message based on strategy
      if (strategy === "3nf" && message === "Table is in BCNF") {
        message = "Table satisfies 3NF";
      }

      const health: TableHealth = {
        tableId,
        severity: (h.thjSeverity === "Critical"
          ? "error"
          : h.thjSeverity === "Warning"
            ? "warning"
            : "ok") as "ok" | "warning" | "error",
        message,
        suggestion: h.thjSuggestion,
      };
      newHealth = HashMap.set(newHealth, tableId, health);
    }
  }
  return newHealth;
};

export const mapMergeSuggestions = (
  backendSuggestions: readonly (readonly [string, string, string])[],
  nameToId: Map<string, TableId>
): MergeSuggestion[] => {
  return backendSuggestions
    .map(([n1, n2, reason]) => {
      const id1 = nameToId.get(n1);
      const id2 = nameToId.get(n2);
      if (!id1 || !id2) {
        return null;
      }
      return { tableId1: id1, tableId2: id2, reason };
    })
    .filter((s): s is NonNullable<typeof s> => s !== null);
};

export const mapSingleHealth = (h: BackendHealth, relationId: TableId): TableHealth => ({
  tableId: relationId,
  severity: (h.hjLevel === "Critical" ? "error" : h.hjLevel === "Warning" ? "warning" : "ok") as
    | "ok"
    | "warning"
    | "error",
  message: h.hjMessage,
  suggestion: h.hjSuggestion,
});
