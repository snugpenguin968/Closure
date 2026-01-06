/**
 * Actions (MVA Update Logic)
 * Pure functions that transition State -> State.
 *
 * DESIGN: All entities use stable UUIDs. Actions are simple and focused.
 * Uses Effect's HashMap for O(1) operations and immutability.
 */

import { HashMap, Option, pipe } from "effect";
import { type WorkspaceState, type History } from "./state";
import {
  type Workspace,
  type Relation,
  type Attribute,
  makeAttribute,
  type TableId,
  makeTableId,
  type MergeSuggestion,
  type CrossTableFDId,
  makeCrossTableFDId,
  type CrossTableFD,
  type FDId,
  makeFDId,
  type TableHealth,
  type SQLType,
  DEFAULT_SQL_TYPE,
} from "./model";

// -- UUID Generation --

const generateTableId = (): TableId => makeTableId(crypto.randomUUID());
const generateFDId = (): FDId => makeFDId(crypto.randomUUID());
const generateCrossTableFDId = (): CrossTableFDId => makeCrossTableFDId(crypto.randomUUID());

// -- History Actions --

const MAX_HISTORY = 50;

export const push = <T>(history: History<T>, newPresent: T): History<T> => {
  if (history.present === newPresent) {
    return history;
  }

  return {
    past: [history.present, ...history.past].slice(0, MAX_HISTORY),
    present: newPresent,
    future: [],
  };
};

export const undo = <T>(history: History<T>): History<T> => {
  if (history.past.length === 0) {
    return history;
  }
  const [previous, ...newPast] = history.past;
  return {
    past: newPast,
    present: previous,
    future: [history.present, ...history.future],
  };
};

export const redo = <T>(history: History<T>): History<T> => {
  if (history.future.length === 0) {
    return history;
  }
  const [next, ...newFuture] = history.future;
  return {
    past: [history.present, ...history.past],
    present: next,
    future: newFuture,
  };
};

// -- Domain Actions (Wrapped in History Push) --

const makeAction =
  (transform: (ws: Workspace) => Workspace) =>
    (state: WorkspaceState): WorkspaceState => {
      const newWorkspace = transform(state.present);
      return push(state, newWorkspace);
    };

// -- HashMap Helpers --

const updateRelation = (
  relations: HashMap.HashMap<TableId, Relation>,
  id: TableId,
  updater: (r: Relation) => Relation
): HashMap.HashMap<TableId, Relation> =>
  pipe(
    HashMap.get(relations, id),
    Option.map((r) => HashMap.set(relations, id, updater(r))),
    Option.getOrElse(() => relations)
  );

// Helper to recalculate cross-table FD suggestions based on shared attributes
const recalculateCrossTableSuggestions = (ws: Workspace): Workspace => {
  const updatedFDs = pipe(
    HashMap.map(ws.crossTableFDs, (ctfd) => {
      const fromRel = HashMap.get(ws.relations, ctfd.fromTableId);
      const toRel = HashMap.get(ws.relations, ctfd.toTableId);

      if (Option.isNone(fromRel) || Option.isNone(toRel)) {
        return ctfd;
      }

      const fromAttrNames = new Set(fromRel.value.attributes.map((a) => a.name));
      const sharedAttrs = toRel.value.attributes.filter((a) => fromAttrNames.has(a.name));

      const suggestion =
        sharedAttrs.length === 0
          ? "No shared keys? Potential join dependency."
          : `Shared: ${sharedAttrs.map((a) => a.name).join(", ")} ✓ Possible FK relationship.`;

      return { ...ctfd, suggestion };
    })
  );

  return { ...ws, crossTableFDs: updatedFDs };
};

// -- Relation CRUD --

export const addRelation = (
  name: string,
  attributes: { name: string; sqlType: SQLType }[],
  fds: { lhs: string[]; rhs: string[] }[],
  x: number,
  y: number
) =>
  makeAction((ws) => {
    const id = generateTableId();
    const newRelation: Relation = {
      id,
      name,
      attributes: attributes.map((a) => ({
        name: makeAttribute(a.name),
        sqlType: a.sqlType,
      })),
      fds: fds.map((fd) => ({
        id: generateFDId(),
        lhs: fd.lhs.map((a) => makeAttribute(a)),
        rhs: fd.rhs.map((a) => makeAttribute(a)),
      })),
      position: { x, y },
    };
    return {
      ...ws,
      relations: HashMap.set(ws.relations, id, newRelation),
    };
  });

export const deleteRelation = (id: TableId) =>
  makeAction((ws) => ({
    ...ws,
    relations: HashMap.remove(ws.relations, id),
    health: HashMap.remove(ws.health, id),
    mergeSuggestions: ws.mergeSuggestions.filter((s) => s.tableId1 !== id && s.tableId2 !== id),
    crossTableFDs: HashMap.filter(
      ws.crossTableFDs,
      (c) => c.fromTableId !== id && c.toTableId !== id
    ),
    foreignKeys: HashMap.filter(
      ws.foreignKeys,
      (fk) => fk.fromTableId !== id && fk.toTableId !== id
    ),
  }));

export const renameRelation = (id: TableId, newName: string) =>
  makeAction((ws) => ({
    ...ws,
    relations: updateRelation(ws.relations, id, (r) => ({ ...r, name: newName })),
  }));

export const addAttribute = (
  relationId: TableId,
  name: string,
  sqlType: SQLType = DEFAULT_SQL_TYPE
) =>
  makeAction((ws) => {
    const updated = {
      ...ws,
      relations: updateRelation(ws.relations, relationId, (r) => ({
        ...r,
        attributes: [...r.attributes, { name: makeAttribute(name), sqlType }],
      })),
    };
    return recalculateCrossTableSuggestions(updated);
  });

export const deleteAttribute = (relationId: TableId, name: string) =>
  makeAction((ws) => {
    const updated = {
      ...ws,
      relations: updateRelation(ws.relations, relationId, (r) => ({
        ...r,
        attributes: r.attributes.filter((a) => a.name !== name),
        fds: r.fds.filter(
          (fd) => !fd.lhs.includes(makeAttribute(name)) && !fd.rhs.includes(makeAttribute(name))
        ),
      })),
    };
    return recalculateCrossTableSuggestions(updated);
  });

export const addFD = (relationId: TableId, lhs: string[], rhs: string[]) =>
  makeAction((ws) => ({
    ...ws,
    relations: updateRelation(ws.relations, relationId, (r) => ({
      ...r,
      fds: [
        ...r.fds,
        {
          id: generateFDId(),
          lhs: lhs.map((a) => makeAttribute(a)),
          rhs: rhs.map((a) => makeAttribute(a)),
        },
      ],
    })),
  }));

export const deleteFD = (relationId: TableId, fdId: FDId) =>
  makeAction((ws) => ({
    ...ws,
    relations: updateRelation(ws.relations, relationId, (r) => ({
      ...r,
      fds: r.fds.filter((fd) => fd.id !== fdId),
    })),
  }));

export const updateRelationPosition = (id: TableId, x: number, y: number) =>
  makeAction((ws) => ({
    ...ws,
    relations: updateRelation(ws.relations, id, (r) => ({ ...r, position: { x, y } })),
  }));

// -- Analysis Results --

export const setAnalysisResults = (
  health: HashMap.HashMap<TableId, TableHealth>,
  mergeSuggestions: MergeSuggestion[],
  analysisWarnings: Workspace["analysisWarnings"]
) =>
  makeAction((ws) => ({
    ...ws,
    health,
    mergeSuggestions,
    analysisWarnings,
  }));

// -- Cross-Table FDs --

export const addCrossTableFD = (fromTableId: TableId, toTableId: TableId) =>
  makeAction((ws) => {
    const fromRel = HashMap.get(ws.relations, fromTableId);
    const toRel = HashMap.get(ws.relations, toTableId);

    let suggestion = "No shared keys? Potential join dependency.";
    if (Option.isSome(fromRel) && Option.isSome(toRel)) {
      const fromAttrs = new Set(fromRel.value.attributes);
      const sharedAttrs = toRel.value.attributes.filter((a) => fromAttrs.has(a));
      if (sharedAttrs.length > 0) {
        suggestion = `Shared: ${sharedAttrs.join(", ")} ✓ Possible FK relationship.`;
      }
    }

    const id = generateCrossTableFDId();
    const newCrossTableFD: CrossTableFD = {
      id,
      fromTableId,
      toTableId,
      fd: { id: generateFDId(), lhs: [], rhs: [] },
      suggestion,
    };

    return {
      ...ws,
      crossTableFDs: HashMap.set(ws.crossTableFDs, id, newCrossTableFD),
    };
  });

export const deleteCrossTableFD = (id: CrossTableFDId) =>
  makeAction((ws) => ({
    ...ws,
    crossTableFDs: HashMap.remove(ws.crossTableFDs, id),
  }));

// -- Merge Relations --

export const mergeRelations = (id1: TableId, id2: TableId) =>
  makeAction((ws) => {
    const r1Opt = HashMap.get(ws.relations, id1);
    const r2Opt = HashMap.get(ws.relations, id2);

    if (Option.isNone(r1Opt) || Option.isNone(r2Opt)) {
      return ws;
    }

    const r1 = r1Opt.value;
    const r2 = r2Opt.value;

    const existingNames = new Set(r1.attributes.map((a) => a.name));
    const uniqueFromR2 = r2.attributes.filter((a) => !existingNames.has(a.name));
    const mergedAttrs = [...r1.attributes, ...uniqueFromR2];
    const mergedFDs = [...r1.fds, ...r2.fds];

    const mergedRel: Relation = {
      id: r1.id,
      name: r1.name,
      attributes: mergedAttrs,
      fds: mergedFDs,
      position: {
        x: (r1.position.x + r2.position.x) / 2,
        y: (r1.position.y + r2.position.y) / 2,
      },
    };

    // Remove both, add merged
    const newRelations = pipe(
      ws.relations,
      (m) => HashMap.remove(m, id1),
      (m) => HashMap.remove(m, id2),
      (m) => HashMap.set(m, r1.id, mergedRel)
    );

    return {
      ...ws,
      relations: newRelations,
      mergeSuggestions: ws.mergeSuggestions.filter(
        (s) => s.tableId1 !== id1 && s.tableId1 !== id2 && s.tableId2 !== id1 && s.tableId2 !== id2
      ),
      health: HashMap.remove(ws.health, id2),
      crossTableFDs: HashMap.filter(
        ws.crossTableFDs,
        (c) => c.fromTableId !== id2 && c.toTableId !== id2
      ),
      foreignKeys: HashMap.filter(
        ws.foreignKeys,
        (fk) => fk.fromTableId !== id2 && fk.toTableId !== id2
      ),
    };
  });

// -- Bulk Operations (for effects layer) --

export const setRelations = (relations: HashMap.HashMap<TableId, Relation>) =>
  makeAction((ws) => ({
    ...ws,
    relations,
  }));

/**
 * Simple relation replacement (kept for backward compatibility).
 */
export const replaceRelation = (oldId: TableId, newRelations: Relation[]) =>
  makeAction((ws) => {
    let updated = HashMap.remove(ws.relations, oldId);
    for (const rel of newRelations) {
      updated = HashMap.set(updated, rel.id, rel);
    }
    return { ...ws, relations: updated };
  });

/**
 * Decompose a relation into multiple tables with edge migration.
 *
 * This action:
 * 1. Removes the old relation
 * 2. Adds all new decomposed relations
 * 3. Migrates inbound edges (crossTableFDs/foreignKeys pointing TO old table)
 * 4. Migrates outbound edges (edges FROM old table)
 * 5. Cleans up health, merge suggestions
 *
 * @param oldId - The table being decomposed
 * @param newRelations - The decomposed tables with positions already set
 * @param inboundMigrations - Map of edge ID → new target table ID
 * @param outboundMigrations - Map of edge ID → new source table ID
 */
export const decomposeRelation = (
  oldId: TableId,
  newRelations: Relation[],
  inboundMigrations: Map<CrossTableFDId, TableId>,
  outboundMigrations: Map<CrossTableFDId, TableId>
) =>
  makeAction((ws) => {
    // 1. Remove old relation, add new ones
    let relations = HashMap.remove(ws.relations, oldId);
    for (const rel of newRelations) {
      relations = HashMap.set(relations, rel.id, rel);
    }

    // 2. Migrate crossTableFDs - update both inbound and outbound edges
    let crossTableFDs = ws.crossTableFDs;

    // Handle inbound edges (TO the decomposed table)
    for (const [edgeId, newTargetId] of inboundMigrations) {
      crossTableFDs = pipe(
        HashMap.get(crossTableFDs, edgeId),
        Option.map((ctfd) =>
          HashMap.set(crossTableFDs, edgeId, {
            ...ctfd,
            toTableId: newTargetId,
            suggestion: recalculateSuggestion(
              HashMap.get(relations, ctfd.fromTableId),
              HashMap.get(relations, newTargetId)
            ),
          })
        ),
        Option.getOrElse(() => crossTableFDs)
      );
    }

    // Handle outbound edges (FROM the decomposed table)
    for (const [edgeId, newSourceId] of outboundMigrations) {
      crossTableFDs = pipe(
        HashMap.get(crossTableFDs, edgeId),
        Option.map((ctfd) =>
          HashMap.set(crossTableFDs, edgeId, {
            ...ctfd,
            fromTableId: newSourceId,
            suggestion: recalculateSuggestion(
              HashMap.get(relations, newSourceId),
              HashMap.get(relations, ctfd.toTableId)
            ),
          })
        ),
        Option.getOrElse(() => crossTableFDs)
      );
    }

    // 3. Remove any edges that still reference the old table (unmigrated)
    crossTableFDs = HashMap.filter(
      crossTableFDs,
      (ctfd) => ctfd.fromTableId !== oldId && ctfd.toTableId !== oldId
    );

    // 4. Migrate foreignKeys similarly
    let foreignKeys = ws.foreignKeys;
    foreignKeys = HashMap.filter(
      foreignKeys,
      (fk) => fk.fromTableId !== oldId && fk.toTableId !== oldId
    );

    // 5. Clean up health for old table
    const health = HashMap.remove(ws.health, oldId);

    // 6. Clean up merge suggestions involving old table
    const mergeSuggestions = ws.mergeSuggestions.filter(
      (s) => s.tableId1 !== oldId && s.tableId2 !== oldId
    );

    return {
      ...ws,
      relations,
      crossTableFDs,
      foreignKeys,
      health,
      mergeSuggestions,
    };
  });

// Helper: Recalculate edge suggestion based on shared attributes
const recalculateSuggestion = (
  fromRelOpt: Option.Option<Relation>,
  toRelOpt: Option.Option<Relation>
): string => {
  if (Option.isNone(fromRelOpt) || Option.isNone(toRelOpt)) {
    return "Migrated (table not found)";
  }

  const fromRel = fromRelOpt.value;
  const toRel = toRelOpt.value;

  const fromAttrs = new Set(fromRel.attributes);
  const sharedAttrs = toRel.attributes.filter((a) => fromAttrs.has(a));

  if (sharedAttrs.length === 0) {
    return "No shared keys? Potential join dependency.";
  }
  return `Shared: ${sharedAttrs.join(", ")} ✓ (migrated)`;
};
