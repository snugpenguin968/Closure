/**
 * Actions (MVA Update Logic)
 * Pure functions that transition State -> State.
 */

import { type WorkspaceState, type History } from "./state";
import { type Workspace, type Relation, type Attribute, type TableId } from "./model";

// -- History Actions --

const MAX_HISTORY = 50;

/**
 * Pushes a new state into history, clearing future.
 */
export const push = <T>(history: History<T>, newPresent: T): History<T> => {
    // If state hasn't changed, return history as-is (optional optimization)
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

// Helper to wrap a pure workspace transform into a history push
const makeAction = (transform: (ws: Workspace) => Workspace) => (state: WorkspaceState): WorkspaceState => {
    const newWorkspace = transform(state.present);
    return push(state, newWorkspace);
};

// Helper to recalculate cross-table FD suggestions based on shared attributes
const recalculateCrossTableSuggestions = (ws: Workspace): Workspace => {
    const updatedFDs = ws.crossTableFDs.map(ctfd => {
        const fromRel = ws.relations.find(r => r.name === ctfd.fromTable || r.id === ctfd.fromTable);
        const toRel = ws.relations.find(r => r.name === ctfd.toTable || r.id === ctfd.toTable);

        if (!fromRel || !toRel) {
            return ctfd; // Keep as-is if relations not found
        }

        // Find shared attributes
        const fromAttrs = new Set(fromRel.attributes);
        const sharedAttrs = toRel.attributes.filter(a => fromAttrs.has(a));

        let suggestion: string;
        if (sharedAttrs.length === 0) {
            suggestion = "No shared keys? Potential join dependency.";
        } else {
            suggestion = `Shared: ${sharedAttrs.join(", ")} ✓ Possible FK relationship.`;
        }

        return { ...ctfd, suggestion };
    });

    return { ...ws, crossTableFDs: updatedFDs };
};

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
            id: name as TableId, // Using name as ID for now, consistent with existing logic
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

export const deleteRelation = (id: string) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.filter((r) => r.id !== id),
    // Cleanup related data
    health: ws.health.filter((h) => h.tableName !== id),
    mergeSuggestions: ws.mergeSuggestions.filter(([t1, t2]) => t1 !== id && t2 !== id),
}));

export const renameRelation = (id: string, newName: string) => makeAction((ws) => {
    // Find the old name for cascading updates
    const oldRel = ws.relations.find((r) => r.id === id);
    if (!oldRel) return ws;
    const oldName = oldRel.name;

    // Helper to replace old name with new name
    const replaceName = (name: string) => (name === oldName ? newName : name);

    return {
        ...ws,
        // Update the relation itself
        relations: ws.relations.map((r) =>
            r.id === id ? { ...r, name: newName, id: newName as TableId } : r
        ),
        // Update crossTableFDs references
        crossTableFDs: ws.crossTableFDs.map((ctfd) => ({
            ...ctfd,
            fromTable: replaceName(ctfd.fromTable),
            toTable: replaceName(ctfd.toTable),
        })),
        // Update health references
        health: ws.health.map((h) => ({
            ...h,
            tableName: replaceName(h.tableName),
        })),
        // Update merge suggestions
        mergeSuggestions: ws.mergeSuggestions.map(([t1, t2, reason]) =>
            [replaceName(t1), replaceName(t2), reason] as const
        ),
        // Update foreign keys
        foreignKeys: ws.foreignKeys.map((fk) => ({
            ...fk,
            fromTable: replaceName(fk.fromTable),
            toTable: replaceName(fk.toTable),
        })),
    };
});

export const addAttribute = (relationId: string, name: string) => makeAction((ws) => {
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

export const deleteAttribute = (relationId: string, name: string) => makeAction((ws) => {
    const updated = {
        ...ws,
        relations: ws.relations.map((r) =>
            r.id === relationId
                ? {
                    ...r,
                    attributes: r.attributes.filter((a) => a !== name),
                    // Cleanup FDs containing this attribute
                    fds: r.fds.filter(
                        (fd) => !fd.lhs.includes(name as Attribute) && !fd.rhs.includes(name as Attribute)
                    ),
                }
                : r
        ),
    };
    return recalculateCrossTableSuggestions(updated);
});

export const addFD = (relationId: string, lhs: string[], rhs: string[]) => makeAction((ws) => ({
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

export const deleteFD = (relationId: string, index: number) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.map((r) =>
        r.id === relationId
            ? { ...r, fds: r.fds.filter((_, i) => i !== index) }
            : r
    ),
}));

export const updateRelationPosition = (id: string, x: number, y: number) => makeAction((ws) => ({
    ...ws,
    relations: ws.relations.map((r) => (r.id === id ? { ...r, position: { x, y } } : r)),
}));

export const setAnalysisResults = (
    _newRelations: Relation[], // Ignored - we keep existing relations
    health: Workspace["health"],
    mergeSuggestions: Workspace["mergeSuggestions"],
    analysisWarnings: Workspace["analysisWarnings"]
) => makeAction((ws) => ({
    // Keep existing relations, only update health/suggestions
    ...ws,
    health,
    mergeSuggestions,
    analysisWarnings,
}));

export const addCrossTableFD = (fromTableId: string, toTableId: string) => makeAction((ws) => ({
    ...ws,
    crossTableFDs: [
        ...ws.crossTableFDs,
        {
            fromTable: fromTableId,
            toTable: toTableId,
            fd: { lhs: [], rhs: [] }, // Placeholder
            suggestion: "No shared keys? Potential join dependency.",
        },
    ],
}));

export const deleteCrossTableFD = (index: number) => makeAction((ws) => ({
    ...ws,
    crossTableFDs: ws.crossTableFDs.filter((_, i) => i !== index),
}));

export const mergeRelations = (name1: string, name2: string) => makeAction((ws) => {
    const r1 = ws.relations.find((r) => r.name === name1);
    const r2 = ws.relations.find((r) => r.name === name2);
    if (!r1 || !r2) return ws;

    const mergedName = name1;
    const mergedAttrs = Array.from(new Set([...r1.attributes, ...r2.attributes]));
    const mergedFDs = [...r1.fds, ...r2.fds];

    const mergedRel: Relation = {
        id: r1.id,
        name: mergedName,
        attributes: mergedAttrs,
        fds: mergedFDs,
        position: {
            x: (r1.position.x + r2.position.x) / 2,
            y: (r1.position.y + r2.position.y) / 2,
        },
    };

    // Get names of tables that will exist after merge
    const remainingTableNames = new Set(
        ws.relations
            .filter((r) => r.name !== name1 && r.name !== name2)
            .map((r) => r.name)
    );
    remainingTableNames.add(mergedName); // Add the merged table

    return {
        ...ws,
        relations: [...ws.relations.filter((r) => r.name !== name1 && r.name !== name2), mergedRel],
        // Filter out any suggestion that references a table that no longer exists
        mergeSuggestions: ws.mergeSuggestions.filter(([t1, t2]) =>
            remainingTableNames.has(t1) && remainingTableNames.has(t2)
        ),
        health: ws.health.filter((h) => h.tableName !== name1 && h.tableName !== name2),
        // Also clean up crossTableFDs that reference the deleted table
        crossTableFDs: ws.crossTableFDs.filter(
            (ctfd) => ctfd.fromTable !== name2 && ctfd.toTable !== name2
        ),
    };
});

