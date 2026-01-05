/**
 * Layout Utilities
 * Pure functions for positioning decomposed tables in a tree structure.
 * No side effects - operates on plain data.
 */

import { type Position } from "./model";

// -- Configuration --

const HORIZONTAL_SPACING = 350;
const VERTICAL_SPACING = 200;

// -- Types --

export interface LayoutNode {
    readonly name: string;
    readonly children: readonly LayoutNode[];
}

export interface LayoutResult {
    readonly name: string;
    readonly position: Position;
}

// -- Tree Layout --

/**
 * Calculate the width of a subtree (number of leaf nodes).
 * Used to determine spacing for child nodes.
 */
const getTreeWidth = (node: LayoutNode): number => {
    if (node.children.length === 0) return 1;
    return node.children.reduce((sum, child) => sum + getTreeWidth(child), 0);
};

/**
 * Layout a decomposition tree with root at anchor position.
 * Children fan out below, centered under their parent.
 * 
 * Visual structure:
 *           [Root]          ← anchor position
 *          /      \
 *     [Child1]  [Child2]    ← VERTICAL_SPACING below
 *        |
 *   [Grandchild]            ← another VERTICAL_SPACING below
 */
export const layoutTree = (root: LayoutNode, anchor: Position): LayoutResult[] => {
    const results: LayoutResult[] = [];

    const layoutNode = (node: LayoutNode, x: number, y: number): void => {
        results.push({ name: node.name, position: { x, y } });

        if (node.children.length === 0) return;

        // Calculate width for each child subtree
        const childWidths = node.children.map(getTreeWidth);
        const totalWidth = childWidths.reduce((a, b) => a + b, 0);

        // Start position: center children under parent
        let currentX = x - ((totalWidth - 1) * HORIZONTAL_SPACING) / 2;
        const childY = y + VERTICAL_SPACING;

        for (let i = 0; i < node.children.length; i++) {
            const child = node.children[i];
            const childWidth = childWidths[i];

            // Center child within its allocated width
            const childX = currentX + ((childWidth - 1) * HORIZONTAL_SPACING) / 2;
            layoutNode(child, childX, childY);

            currentX += childWidth * HORIZONTAL_SPACING;
        }
    };

    layoutNode(root, anchor.x, anchor.y);
    return results;
};

// -- Grid Layout (Fallback) --

/**
 * Simple grid layout when no tree structure is available.
 * Used as fallback when backend doesn't return decomposition tree.
 */
export const layoutGrid = (
    names: readonly string[],
    anchor: Position,
    columns: number = 3
): LayoutResult[] => {
    return names.map((name, i) => ({
        name,
        position: {
            x: anchor.x + (i % columns) * HORIZONTAL_SPACING,
            y: anchor.y + Math.floor(i / columns) * VERTICAL_SPACING,
        },
    }));
};

// -- Helpers --

/**
 * Get configuration values for external use.
 */
export const getLayoutConfig = () => ({
    horizontalSpacing: HORIZONTAL_SPACING,
    verticalSpacing: VERTICAL_SPACING,
});
