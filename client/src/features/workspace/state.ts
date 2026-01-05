/**
 * State (MVA Model Definition)
 * Pure data definitions for Application State.
 */

import { HashMap } from "effect";
import {
  type Workspace,
  type TableId,
  type Relation,
  type CrossTableFDId,
  type CrossTableFD,
  type ForeignKeyId,
  type ForeignKey,
  type TableHealth,
} from "./model";

// -- History (Undo/Redo) --

export interface History<T> {
  readonly past: ReadonlyArray<T>; // Stack of past states
  readonly present: T; // Current state
  readonly future: ReadonlyArray<T>; // Stack of future states (for redo)
}

export const createHistory = <T>(present: T): History<T> => ({
  past: [],
  present,
  future: [],
});

// -- Workspace State --

export type WorkspaceState = History<Workspace>;

export const initialWorkspace: Workspace = {
  relations: HashMap.empty<TableId, Relation>(),
  crossTableFDs: HashMap.empty<CrossTableFDId, CrossTableFD>(),
  foreignKeys: HashMap.empty<ForeignKeyId, ForeignKey>(),
  health: HashMap.empty<TableId, TableHealth>(),
  mergeSuggestions: [],
  analysisWarnings: [],
};

export const initialState: WorkspaceState = createHistory(initialWorkspace);
