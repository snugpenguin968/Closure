/**
 * Actions (MVA Update Logic)
 * Pure functions that transition State -> State.
 * 
 * DESIGN: All references use TableId (UUID). Actions are simple and focused.
 * No cascade updates needed because IDs are stable.
 */

import { type WorkspaceState, type History } from "./state";
import { type Workspace, type Relation, type Attribute, type TableId, type MergeSuggestion } from "./model";

// -- UUID Generation --

const generateId = (): TableId => crypto.randomUUID() as TableId;

// -- History Actions --

const MAX_HISTORY = 50;

/**
 * Pushes a new state into history, clearing future.
 */
export const push = <T>(history: History<T>, newPresent: T): History<T> => {
    if (history.present === newPresent) return history;

    return {
        past: [history.present, ...history.past].slice(0, MAX_HISTORY),
        present: newPresent,
        future: [],
    };
};

export const undo = <T>(history: History<T>): History<T> => {
    if (history.past.length === 0) return history;
    const [previous, ...newPast] = history.past;
    return {
        past: newPast,
        present: previous,
        future: [history.present, ...history.future],
    };
};

export const redo = <T>(history: History<T>): History<T> => {
    if (history.future.length === 0) return history;
    const [next, ...newFuture] = history.future;
    return {
        past: [history.present, ...history.past],
        present: next,
        future: newFuture,
    };
};

// -- Domain Actions (Wrapped in History Push) --

const makeAction = (transform: (ws: Workspace) => Workspace) => (state: WorkspaceState): WorkspaceState => {
    const newWorkspace = transform(state.present);
    return push(state, newWorkspace);
};

// Helper to recalculate cross-table FD suggestions based on shared attributes
const recalculateCrossTableSuggestions = (ws: Workspace): Workspace => {
    const relMap = new Map(ws.relations.map(r => [r.id, r]));

    const updatedFDs = ws.crossTableFDs.map(ctfd => {
        const fromRel = relMap.get(ctfd.fromTableId);
        const toRel = relMap.get(ctfd.toTableId);

        if (!fromRel || !toRel) {
            return ctfd;
        }

        const fromAttrs = new Set(fromRel.attributes);
        const sharedAttrs = toRel.attributes.filter(a => fromAttrs.has(a));

        const suggestion = sharedAttrs.length === 0
            ? "No shared keys? Potential join dependency."
            : `Shared: ${sharedAttrs.join(", ")} ✓ Possible FK relationship.`;

        return { ...ctfd, suggestion };
    });

    return { ...ws, crossTableFDs: updatedFDs };
};

// -- Relation CRUD --

export const addRelation = (
    name: string,
    attributes: string[],
    fds: { lhs: string[]; rhs: string[] }[],
    x: number,
    y: number
) => makeAction((ws) => ({
    ...ws,
    relations: [
        ...ws.relations,
        {
            id: generateId(),
            name,
            attributes: attributes.map((a) => a as Attribute),
            fds: fds.map((fd) => ({
                lhs: fd.lhs.map((a) => a as Attribute),
                rhs: fd.rhs.map((a) => a as Attribute),
            })),
            position: { x, y },
        },
    ],
}));

export const deleteRelation = (id: TableId) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.filter((r) => r.id !== id),
    // Clean up all references by ID
    health: ws.health.filter((h) => h.tableId !== id),
    mergeSuggestions: ws.mergeSuggestions.filter((s) => s.tableId1 !== id && s.tableId2 !== id),
    crossTableFDs: ws.crossTableFDs.filter((c) => c.fromTableId !== id && c.toTableId !== id),
    foreignKeys: ws.foreignKeys.filter((fk) => fk.fromTableId !== id && fk.toTableId !== id),
}));

// SIMPLE! Just update the name. No cascade needed.
export const renameRelation = (id: TableId, newName: string) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.map((r) => r.id === id ? { ...r, name: newName } : r),
}));

export const addAttribute = (relationId: TableId, name: string) => makeAction((ws) => {
    const updated = {
        ...ws,
        relations: ws.relations.map((r) =>
            r.id === relationId
                ? { ...r, attributes: [...r.attributes, name as Attribute] }
                : r
        ),
    };
    return recalculateCrossTableSuggestions(updated);
});

export const deleteAttribute = (relationId: TableId, name: string) => makeAction((ws) => {
    const updated = {
        ...ws,
        relations: ws.relations.map((r) =>
            r.id === relationId
                ? {
                    ...r,
                    attributes: r.attributes.filter((a) => a !== name),
                    fds: r.fds.filter(
                        (fd) => !fd.lhs.includes(name as Attribute) && !fd.rhs.includes(name as Attribute)
                    ),
                }
                : r
        ),
    };
    return recalculateCrossTableSuggestions(updated);
});

export const addFD = (relationId: TableId, lhs: string[], rhs: string[]) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.map((r) =>
        r.id === relationId
            ? {
                ...r,
                fds: [
                    ...r.fds,
                    {
                        lhs: lhs.map((a) => a as Attribute),
                        rhs: rhs.map((a) => a as Attribute),
                    },
                ],
            }
            : r
    ),
}));

export const deleteFD = (relationId: TableId, index: number) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.map((r) =>
        r.id === relationId
            ? { ...r, fds: r.fds.filter((_, i) => i !== index) }
            : r
    ),
}));

export const updateRelationPosition = (id: TableId, x: number, y: number) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.map((r) => (r.id === id ? { ...r, position: { x, y } } : r)),
}));

// -- Analysis Results --

export const setAnalysisResults = (
    health: Workspace["health"],
    mergeSuggestions: MergeSuggestion[],
    analysisWarnings: Workspace["analysisWarnings"]
) => makeAction((ws) => ({
    ...ws,
    health,
    mergeSuggestions,
    analysisWarnings,
}));

// -- Cross-Table FDs --

export const addCrossTableFD = (fromTableId: TableId, toTableId: TableId) => makeAction((ws) => {
    const fromRel = ws.relations.find(r => r.id === fromTableId);
    const toRel = ws.relations.find(r => r.id === toTableId);

    // Calculate initial suggestion
    let suggestion = "No shared keys? Potential join dependency.";
    if (fromRel && toRel) {
        const fromAttrs = new Set(fromRel.attributes);
        const sharedAttrs = toRel.attributes.filter(a => fromAttrs.has(a));
        if (sharedAttrs.length > 0) {
            suggestion = `Shared: ${sharedAttrs.join(", ")} ✓ Possible FK relationship.`;
        }
    }

    return {
        ...ws,
        crossTableFDs: [
            ...ws.crossTableFDs,
            {
                fromTableId,
                toTableId,
                fd: { lhs: [], rhs: [] },
                suggestion,
            },
        ],
    };
});

export const deleteCrossTableFD = (index: number) => makeAction((ws) => ({
    ...ws,
    crossTableFDs: ws.crossTableFDs.filter((_, i) => i !== index),
}));

// -- Merge Relations --

export const mergeRelations = (id1: TableId, id2: TableId) => makeAction((ws) => {
    const r1 = ws.relations.find((r) => r.id === id1);
    const r2 = ws.relations.find((r) => r.id === id2);
    if (!r1 || !r2) return ws;

    const mergedAttrs = Array.from(new Set([...r1.attributes, ...r2.attributes]));
    const mergedFDs = [...r1.fds, ...r2.fds];

    const mergedRel: Relation = {
        id: r1.id,  // Keep first table's ID
        name: r1.name,  // Keep first table's name
        attributes: mergedAttrs,
        fds: mergedFDs,
        position: {
            x: (r1.position.x + r2.position.x) / 2,
            y: (r1.position.y + r2.position.y) / 2,
        },
    };

    return {
        ...ws,
        relations: [...ws.relations.filter((r) => r.id !== id1 && r.id !== id2), mergedRel],
        // Remove suggestions involving either table
        mergeSuggestions: ws.mergeSuggestions.filter((s) =>
            s.tableId1 !== id1 && s.tableId1 !== id2 && s.tableId2 !== id1 && s.tableId2 !== id2
        ),
        // Remove health for merged-away table
        health: ws.health.filter((h) => h.tableId !== id2),
        // Update crossTableFDs: remove ones to deleted table, update refs to merged table
        crossTableFDs: ws.crossTableFDs
            .filter((c) => c.fromTableId !== id2 && c.toTableId !== id2)
            .map((c) => ({
                ...c,
                // Keep using id1 for any that referenced id1
            })),
        // Update foreignKeys similarly
        foreignKeys: ws.foreignKeys.filter((fk) => fk.fromTableId !== id2 && fk.toTableId !== id2),
    };
});
