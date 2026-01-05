/**
 * Link Migration Utilities
 * Pure functions for handling edge migration when tables are decomposed.
 * Determines how to reroute edges when a table is split into multiple tables.
 */

import { type Relation, type TypedAttribute, type TableId } from "./model";

// -- Types --

export interface MigrationResult {
  readonly newTableId: TableId;
  readonly sharedCount: number;
  readonly sharedAttributeNames: readonly string[];
}

// -- Core Algorithm --

/**
 * Find the best destination table for an edge that pointed to a decomposed table.
 *
 * Strategy: Route to the decomposed table that has the most shared attributes
 * with the source table. This preserves the semantic relationship.
 */
export const findBestDestination = (
  sourceAttributes: readonly TypedAttribute[],
  decomposedTables: readonly Relation[]
): MigrationResult | null => {
  if (decomposedTables.length === 0) {
    return null;
  }

  const sourceNames = new Set(sourceAttributes.map((a) => a.name));

  let bestMatch: MigrationResult | null = null;

  for (const table of decomposedTables) {
    const sharedAttrs = table.attributes.filter((a) => sourceNames.has(a.name));
    const sharedCount = sharedAttrs.length;

    if (sharedCount > 0) {
      if (!bestMatch || sharedCount > bestMatch.sharedCount) {
        bestMatch = {
          newTableId: table.id,
          sharedCount,
          sharedAttributeNames: sharedAttrs.map((a) => a.name),
        };
      }
    }
  }

  return bestMatch;
};

/**
 * Find destination for outgoing edges (edges FROM the decomposed table).
 *
 * When a table is decomposed, edges going OUT from it should be routed
 * to the decomposed table that contains the relevant FK attributes.
 */
export const findSourceForOutgoingEdge = (
  targetAttributes: readonly TypedAttribute[],
  decomposedTables: readonly Relation[]
): MigrationResult | null => {
  return findBestDestination(targetAttributes, decomposedTables);
};

// -- Suggestion Text --

/**
 * Generate a human-readable suggestion for a migrated edge.
 */
export const generateMigrationSuggestion = (result: MigrationResult): string => {
  if (result.sharedCount === 0) {
    return "Migrated (no shared attributes found)";
  }
  return `Shared: ${result.sharedAttributeNames.join(", ")} ✓ (migrated)`;
};
