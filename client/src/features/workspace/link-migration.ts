/**
 * Link Migration Utilities
 * Pure functions for handling edge migration when tables are decomposed.
 * Determines how to reroute edges when a table is split into multiple tables.
 */

import { type Relation, type Attribute, type TableId } from "./model";

// -- Types --

export interface MigrationResult {
  readonly newTableId: TableId;
  readonly sharedCount: number;
  readonly sharedAttributes: readonly Attribute[];
}

// -- Core Algorithm --

/**
 * Find the best destination table for an edge that pointed to a decomposed table.
 *
 * Strategy: Route to the decomposed table that has the most shared attributes
 * with the source table. This preserves the semantic relationship.
 *
 * Example:
 *   Source: Orders(order_id, customer_id, order_date)
 *   Decomposed tables:
 *     - Customer_Core(customer_id, name)      ← 1 shared attr (customer_id)
 *     - Customer_Address(customer_id, address) ← 1 shared attr
 *
 *   Both have 1 shared attribute, so we pick the first one (Customer_Core)
 *   which is typically the "main" table in the decomposition.
 */
export const findBestDestination = (
  sourceAttributes: readonly Attribute[],
  decomposedTables: readonly Relation[]
): MigrationResult | null => {
  if (decomposedTables.length === 0) {
    return null;
  }

  const sourceSet = new Set(sourceAttributes);

  let bestMatch: MigrationResult | null = null;

  for (const table of decomposedTables) {
    const sharedAttributes = table.attributes.filter((a) => sourceSet.has(a));
    const sharedCount = sharedAttributes.length;

    if (sharedCount > 0) {
      if (!bestMatch || sharedCount > bestMatch.sharedCount) {
        bestMatch = {
          newTableId: table.id,
          sharedCount,
          sharedAttributes,
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
  targetAttributes: readonly Attribute[],
  decomposedTables: readonly Relation[]
): MigrationResult | null => {
  // Same algorithm - find table with most shared attributes
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
  return `Shared: ${result.sharedAttributes.join(", ")} ✓ (migrated)`;
};
