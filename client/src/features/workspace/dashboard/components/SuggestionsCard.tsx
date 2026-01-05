import React from "react";
import { Card, CardHeader, CardTitle, CardContent } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
interface MergeSuggestionDisplay {
  id1: string;
  id2: string;
  name1: string;
  name2: string;
  reason: string;
}

export const SuggestionsCard = ({
  suggestions,
  onClose,
  onApply,
}: {
  suggestions: readonly MergeSuggestionDisplay[];
  onClose: () => void;
  onApply: (id1: string, id2: string) => void;
}): React.ReactElement => (
  <Card className="w-80 shadow-xl pointer-events-auto bg-white/95 backdrop-blur border-indigo-100">
    <CardHeader className="p-3 border-b border-indigo-50 bg-gradient-to-r from-indigo-50 to-purple-50 flex flex-row items-center justify-between space-y-0">
      <CardTitle className="text-sm font-medium flex items-center gap-2 text-indigo-900">
        💡 Intelligent Suggestions
      </CardTitle>
      <button onClick={onClose} className="text-indigo-400 hover:text-indigo-600 transition-colors">
        ×
      </button>
    </CardHeader>
    <CardContent className="p-3 max-h-60 overflow-y-auto">
      <div className="flex flex-col gap-2">
        {suggestions.map((s, i) => (
          <div
            key={i}
            className="text-xs p-2 bg-indigo-50 border border-indigo-100 rounded text-indigo-900 flex justify-between items-center"
          >
            <div>
              <span className="font-bold">{s.name1}</span>
              <span className="mx-1 text-indigo-400">→</span>
              <span className="font-bold">{s.name2}</span>
              <p className="text-slate-600 mt-1">{s.reason}</p>
            </div>
            <Button
              size="sm"
              className="h-6 px-2 text-[10px] bg-indigo-200 text-indigo-800 hover:bg-indigo-300 ml-2 shadow-none border border-indigo-300"
              onClick={() => onApply(s.id1, s.id2)}
            >
              Merge
            </Button>
          </div>
        ))}
      </div>
    </CardContent>
  </Card>
);
