/**
 * View.Optimizer (The View)
 * Sidebar UI for selecting strategy.
 */

"use client";

import React from "react";
import {
  Sheet,
  SheetContent,
  SheetHeader,
  SheetTitle,
  SheetDescription,
  SheetFooter,
} from "@/components/ui/sheet";
import { Slider } from "@/components/ui/slider";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import { Badge } from "@/components/ui/badge";
import { Option } from "effect";
import { type TreeNode, type TableHealth, type OptimizationStrategy } from "../workspace/model";

export interface OptimizerViewProps {
  isOpen: boolean;
  onOpenChange: (open: boolean) => void;
  selectedStrategy: OptimizationStrategy;
  onStrategyChange: (strategy: OptimizationStrategy) => void;
  onAutoFix: () => void;
  targetRelationId?: string;
  latestAnalysis: Option.Option<{
    readonly tree: Option.Option<TreeNode>;
    readonly health: Option.Option<TableHealth>;
    readonly warnings: readonly string[];
  }>;
}

const DecompositionTreeItem = ({ node }: { node: TreeNode }) => {
  return (
    <div className="flex flex-col gap-2 ml-4">
      <div className="flex items-center gap-2 border-l-2 border-slate-200 pl-2 py-1">
        <Badge variant="outline" className="text-xs font-mono">
          {node.relation.name}
        </Badge>
        <span className="text-xs text-slate-400">({node.relation.attributes.join(", ")})</span>
      </div>
      {Option.isSome(node.splitFD) && (
        <div className="pl-4 text-xs text-amber-600 font-mono">
          ✂️ {node.splitFD.value.lhs.join(",")} ➔ {node.splitFD.value.rhs.join(",")}
        </div>
      )}
      <div className="flex flex-col gap-1">
        {node.children.map((child, i) => (
          <DecompositionTreeItem key={i} node={child} />
        ))}
      </div>
    </div>
  );
};

export const OptimizerView: React.FC<OptimizerViewProps> = ({
  isOpen,
  onOpenChange,
  selectedStrategy,
  onStrategyChange,
  onAutoFix,
  targetRelationId,
  latestAnalysis,
}) => {
  const strategyValue = selectedStrategy === "bcnf" ? 0 : selectedStrategy === "3nf" ? 50 : 100;

  const handleSliderChange = (vals: number[]) => {
    const v = vals[0];
    if (v < 33) {
      onStrategyChange("bcnf");
    } else if (v < 66) {
      onStrategyChange("3nf");
    } else {
      onStrategyChange("performance");
    }
  };

  return (
    <Sheet open={isOpen} onOpenChange={onOpenChange}>
      <SheetContent className="sm:max-w-md overflow-y-auto">
        <SheetHeader>
          <SheetTitle>Optimizer</SheetTitle>
          <SheetDescription>Optimize table structure for {targetRelationId}</SheetDescription>
        </SheetHeader>

        <div className="grid gap-6 py-6">
          <div className="flex flex-col gap-4">
            <div className="flex justify-between items-center">
              <Label>Optimization Goal</Label>
              <Badge variant={selectedStrategy === "performance" ? "destructive" : "secondary"}>
                {selectedStrategy === "bcnf" && "Pure Integrity (BCNF)"}
                {selectedStrategy === "3nf" && "Balanced (3NF)"}
                {selectedStrategy === "performance" && "High Performance"}
              </Badge>
            </div>

            <Slider
              min={0}
              max={100}
              step={50}
              value={[strategyValue]}
              onValueChange={handleSliderChange}
              className="py-4"
            />

            <div className="flex justify-between text-xs text-slate-500 px-1">
              <span>Integrity</span>
              <span>Balanced</span>
              <span>Speed</span>
            </div>
          </div>

          <div className="bg-slate-50 p-4 rounded-lg border border-slate-100 text-sm space-y-2">
            <p className="font-medium text-slate-700">Analysis</p>

            {Option.isSome(latestAnalysis) ? (
              <div className="flex flex-col gap-3">
                {Option.map(latestAnalysis.value.health, (h) => (
                  <div
                    key="health"
                    className={`p-2 rounded border ${h.severity === "error" ? "bg-red-50 border-red-200 text-red-700" : "bg-green-50 border-green-200 text-green-700"}`}
                  >
                    <div className="font-bold text-xs uppercase">{h.severity}</div>
                    <div>{h.message}</div>
                  </div>
                )).pipe(Option.getOrNull)}

                {latestAnalysis.value.warnings.length > 0 && (
                  <div className="text-xs text-amber-600 bg-amber-50 p-2 rounded border border-amber-100">
                    <div className="font-bold">Warnings</div>
                    <ul className="list-disc list-inside">
                      {latestAnalysis.value.warnings.map((w, i) => (
                        <li key={i}>{w}</li>
                      ))}
                    </ul>
                  </div>
                )}

                {Option.map(latestAnalysis.value.tree, (tree) => (
                  <div key="tree" className="mt-2">
                    <div className="text-xs font-bold text-slate-500 uppercase mb-2">
                      Decomposition Tree
                    </div>
                    <div className="border-l-2 border-slate-200 pl-1">
                      <DecompositionTreeItem node={tree} />
                    </div>
                  </div>
                )).pipe(Option.getOrNull)}
              </div>
            ) : (
              <p className="text-slate-500 italic">
                No analysis results yet. Run Auto-Fix to analyze.
              </p>
            )}
          </div>
        </div>

        <SheetFooter>
          <Button onClick={onAutoFix} className="w-full">
            Run Optimizer
          </Button>
        </SheetFooter>
      </SheetContent>
    </Sheet>
  );
};
