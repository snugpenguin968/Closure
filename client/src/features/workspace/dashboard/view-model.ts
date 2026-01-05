import { HashMap } from "effect";
import { type Workspace } from "../model";

// -- View Models --

export interface MergeSuggestionDisplay {
  id1: string;
  id2: string;
  name1: string;
  name2: string;
  reason: string;
}

// -- Factory --

export const mapSuggestionsToDisplay = (workspace: Workspace): MergeSuggestionDisplay[] => {
  const idToName = new Map<string, string>();
  for (const rel of HashMap.values(workspace.relations)) {
    idToName.set(rel.id, rel.name);
  }

  // Build set of edges for fast lookup of directionality
  const edges = new Set<string>();
  for (const ctfd of HashMap.values(workspace.crossTableFDs)) {
    edges.add(`${ctfd.fromTableId}->${ctfd.toTableId}`);
  }

  const seenPairs = new Set<string>();
  const result: MergeSuggestionDisplay[] = [];

  for (const s of workspace.mergeSuggestions) {
    // Deduplicate pairs (ignore order for key)
    const pairKey = [s.tableId1, s.tableId2].sort().join(":");
    if (seenPairs.has(pairKey)) {
      continue;
    }
    seenPairs.add(pairKey);

    let id1 = s.tableId1;
    let id2 = s.tableId2;
    let name1 = idToName.get(id1);
    let name2 = idToName.get(id2);

    if (!name1 || !name2) {
      continue;
    }

    // Determine orientation based on edges (Prefer A->B)
    // If B->A exists, we swap them so we can display "Merge B into A" (Source -> Target naming convention)
    // OR "Merge A into B".
    // Let's assume user wants "Merge From into To".
    if (edges.has(`${id2}->${id1}`)) {
      [id1, id2] = [id2, id1];
      [name1, name2] = [name2, name1];
    }

    result.push({
      id1,
      id2,
      name1,
      name2,
      reason: s.reason,
    });
  }

  return result;
};
