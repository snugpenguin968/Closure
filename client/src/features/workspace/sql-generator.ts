import { HashMap } from "effect";
import {
  type Workspace,
  type Relation,
  type TableId,
  type TypedAttribute,
  type FunctionalDependency,
  type ForeignKey,
} from "./model";

export interface SQLGeneratorOptions {
  readonly dialect: "postgres" | "mysql" | "sqlite";
  readonly includeIndexes: boolean;
}

const defaultOptions: SQLGeneratorOptions = {
  dialect: "postgres",
  includeIndexes: true,
};

const toSnakeCase = (str: string): string =>
  str
    .replace(/([A-Z])/g, "_$1")
    .toLowerCase()
    .replace(/^_/, "")
    .replace(/\s+/g, "_")
    .replace(/-/g, "_")
    .replace(/__+/g, "_");

const quote = (identifier: string): string => `"${toSnakeCase(identifier)}"`;

const computeClosure = (
  attrs: ReadonlySet<string>,
  fds: readonly FunctionalDependency[]
): Set<string> => {
  const closure = new Set(attrs);
  let changed = true;
  while (changed) {
    changed = false;
    for (const fd of fds) {
      if (fd.lhs.every((a) => closure.has(a))) {
        for (const r of fd.rhs) {
          if (!closure.has(r)) {
            closure.add(r);
            changed = true;
          }
        }
      }
    }
  }
  return closure;
};

const derivePrimaryKey = (relation: Relation): readonly TypedAttribute[] => {
  const attrNames = relation.attributes.map((a) => a.name);
  const allAttrs = new Set(attrNames);
  const fds = relation.fds;

  if (fds.length === 0) {
    const idAttr = relation.attributes.find((a) => toSnakeCase(a.name) === "id");
    return idAttr ? [idAttr] : relation.attributes.slice(0, 1);
  }

  for (const fd of fds) {
    const lhsSet = new Set(fd.lhs);
    const closure = computeClosure(lhsSet, fds);
    if ([...allAttrs].every((a) => closure.has(a))) {
      return relation.attributes.filter((attr) => fd.lhs.includes(attr.name));
    }
  }

  for (let size = 1; size <= relation.attributes.length; size++) {
    const combos = combinations(relation.attributes, size);
    for (const combo of combos) {
      const comboNames = new Set(combo.map((a) => a.name));
      const closure = computeClosure(comboNames, fds);
      if ([...allAttrs].every((a) => closure.has(a))) {
        return combo;
      }
    }
  }

  return relation.attributes.slice(0, 1);
};

function* combinations<T>(arr: readonly T[], size: number): Generator<T[]> {
  if (size === 0) {
    yield [];
    return;
  }
  if (arr.length < size) {
    return;
  }
  const [first, ...rest] = arr;
  for (const combo of combinations(rest, size - 1)) {
    yield [first, ...combo];
  }
  yield* combinations(rest, size);
}

const generateColumnDef = (attr: TypedAttribute, primaryKey: readonly TypedAttribute[]): string => {
  const colName = quote(attr.name);
  const colType = attr.sqlType;
  const isInPK = primaryKey.some((pk) => pk.name === attr.name);
  const notNull = isInPK ? " NOT NULL" : "";
  return `    ${colName} ${colType}${notNull}`;
};

const generateTableSQL = (relation: Relation): string => {
  const tableName = quote(relation.name);
  const primaryKey = derivePrimaryKey(relation);

  const columnDefs = relation.attributes.map((attr) => generateColumnDef(attr, primaryKey));
  const pkColumns = primaryKey.map((a) => quote(a.name)).join(", ");
  const pkConstraint = `    PRIMARY KEY (${pkColumns})`;

  const body = [...columnDefs, pkConstraint].join(",\n");
  return `CREATE TABLE ${tableName} (\n${body}\n);\n`;
};

const generateForeignKeySQL = (fk: ForeignKey, idToName: Map<TableId, string>): string | null => {
  const fromName = idToName.get(fk.fromTableId);
  const toName = idToName.get(fk.toTableId);
  if (!fromName || !toName) {
    return null;
  }

  const fromTable = quote(fromName);
  const toTable = quote(toName);
  const fromCol = quote(fk.fromAttribute);
  const toCol = quote(fk.toAttribute);
  const constraintName = `fk_${toSnakeCase(fromName)}_${toSnakeCase(fk.fromAttribute)}`;

  return `ALTER TABLE ${fromTable} ADD CONSTRAINT "${constraintName}" FOREIGN KEY (${fromCol}) REFERENCES ${toTable} (${toCol});`;
};

const generateIndexSQL = (fk: ForeignKey, idToName: Map<TableId, string>): string | null => {
  const fromName = idToName.get(fk.fromTableId);
  if (!fromName) {
    return null;
  }

  const tableName = quote(fromName);
  const colName = quote(fk.fromAttribute);
  const indexName = `idx_${toSnakeCase(fromName)}_${toSnakeCase(fk.fromAttribute)}`;

  return `CREATE INDEX "${indexName}" ON ${tableName} (${colName});`;
};

export const generateSQL = (
  workspace: Workspace,
  options: Partial<SQLGeneratorOptions> = {}
): string => {
  const opts = { ...defaultOptions, ...options };

  const relationsArray = Array.from(HashMap.values(workspace.relations));
  const idToName = new Map<TableId, string>();
  for (const rel of relationsArray) {
    idToName.set(rel.id, rel.name);
  }

  const tables = relationsArray.map(generateTableSQL);

  const foreignKeysArray = Array.from(HashMap.values(workspace.foreignKeys));
  const fkStatements = foreignKeysArray
    .map((fk) => generateForeignKeySQL(fk, idToName))
    .filter((s): s is string => s !== null);

  const indexStatements = opts.includeIndexes
    ? foreignKeysArray
        .map((fk) => generateIndexSQL(fk, idToName))
        .filter((s): s is string => s !== null)
    : [];

  const sections: string[] = [
    "-- Schema Export",
    `-- Generated: ${new Date().toISOString()}`,
    "",
    "-- Tables",
    ...tables,
  ];

  if (fkStatements.length > 0) {
    sections.push("-- Foreign Keys", ...fkStatements, "");
  }

  if (indexStatements.length > 0) {
    sections.push("-- Indexes", ...indexStatements, "");
  }

  return sections.join("\n");
};
