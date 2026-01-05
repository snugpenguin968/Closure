import React, { useState } from "react";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { ArrowDown } from "lucide-react";

export const RelationshipBuilder = ({
  attributes,
  onSave,
  onCancel,
}: {
  attributes: readonly string[];
  onSave: (lhs: string[], rhs: string[]) => void;
  onCancel: () => void;
}): React.ReactElement => {
  const [lhs, setLhs] = useState<Set<string>>(new Set());
  const [rhs, setRhs] = useState<Set<string>>(new Set());

  const toggleLhs = (attr: string): void => {
    const next = new Set(lhs);
    if (next.has(attr)) {
      next.delete(attr);
    } else {
      next.add(attr);
    }
    setLhs(next);
  };

  const toggleRhs = (attr: string): void => {
    const next = new Set(rhs);
    if (next.has(attr)) {
      next.delete(attr);
    } else {
      next.add(attr);
    }
    setRhs(next);
  };

  const handleSave = (): void => {
    if (lhs.size > 0 && rhs.size > 0) {
      onSave(Array.from(lhs), Array.from(rhs));
    }
  };

  return (
    <div className="p-3 bg-slate-50 rounded-md border border-slate-200 space-y-3">
      <div className="text-xs font-semibold text-slate-700">Define Dependency</div>

      <div className="space-y-1">
        <div className="text-[10px] uppercase text-slate-500 font-bold tracking-wider">
          Determinants
        </div>
        <div className="flex flex-wrap gap-1">
          {attributes.map((attr) => (
            <Badge
              key={attr}
              variant={lhs.has(attr) ? "default" : "outline"}
              className="cursor-pointer hover:bg-slate-200 data-[state=active]:hover:bg-primary/90"
              data-state={lhs.has(attr) ? "active" : "inactive"}
              onClick={() => toggleLhs(attr)}
            >
              {attr}
            </Badge>
          ))}
        </div>
      </div>

      <div className="flex justify-center text-slate-400">
        <ArrowDown className="w-4 h-4" />
      </div>

      <div className="space-y-1">
        <div className="text-[10px] uppercase text-slate-500 font-bold tracking-wider">
          Dependents
        </div>
        <div className="flex flex-wrap gap-1">
          {attributes.map((attr) => (
            <Badge
              key={attr}
              variant="outline"
              className={`cursor-pointer hover:bg-slate-200 ${
                rhs.has(attr)
                  ? "bg-amber-100 text-amber-800 border-amber-200 hover:bg-amber-200"
                  : ""
              }`}
              onClick={() => toggleRhs(attr)}
            >
              {attr}
            </Badge>
          ))}
        </div>
      </div>

      <div className="flex justify-end gap-2 pt-2 border-t border-slate-200">
        <Button size="sm" variant="ghost" onClick={onCancel} className="h-6 px-2 text-xs">
          Cancel
        </Button>
        <Button
          size="sm"
          onClick={handleSave}
          disabled={lhs.size === 0 || rhs.size === 0}
          className="h-6 px-2 text-xs bg-indigo-600 hover:bg-indigo-700 text-white"
        >
          Add Rule
        </Button>
      </div>
    </div>
  );
};
