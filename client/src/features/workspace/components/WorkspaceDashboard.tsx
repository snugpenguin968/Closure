import React, { useState } from "react";
import { Button } from "@/components/ui/button";
import { Card, CardHeader, CardTitle, CardContent } from "@/components/ui/card";
import { type MergeSuggestionDisplay } from "../view-model";
import { SuggestionsCard } from "./SuggestionsCard";

export const WorkspaceDashboard = ({
    onOptimize,
    onAddTable,
    onExportSQL,
    suggestions,
    isAnalyzing,
    analysisWarnings,
    onMerge,
}: {
    onOptimize: () => void;
    onAddTable: () => void;
    onExportSQL: () => void;
    suggestions: readonly MergeSuggestionDisplay[];
    isAnalyzing?: boolean;
    analysisWarnings: readonly string[];
    onMerge: (id1: string, id2: string) => void;
}): React.ReactElement => {
    const [showSuggestions, setShowSuggestions] = useState(true);
    const [lastSuggestions, setLastSuggestions] = useState(suggestions);

    // Reset showSuggestions when new suggestions arrive
    if (suggestions !== lastSuggestions) {
        setLastSuggestions(suggestions);
        if (suggestions.length > 0) {
            setShowSuggestions(true);
        }
    }

    return (
        <div className="absolute top-4 right-4 z-10 flex flex-col gap-4 items-end pointer-events-none">
            <div className="pointer-events-auto flex gap-2">
                <Button
                    onClick={onAddTable}
                    variant="secondary"
                    className="shadow-lg bg-white hover:bg-slate-100 text-slate-700 border border-slate-200"
                >
                    + New Table
                </Button>
                <Button
                    variant="outline"
                    className="shadow-lg bg-white/90 backdrop-blur hover:bg-slate-50 text-slate-700 border-slate-200"
                    onClick={onExportSQL}
                >
                    💾 SQL
                </Button>
                <Button
                    onClick={onOptimize}
                    disabled={isAnalyzing}
                    className="shadow-lg bg-gradient-to-r from-indigo-600 to-purple-600 hover:from-indigo-700 hover:to-purple-700 text-white disabled:opacity-70"
                >
                    {isAnalyzing ? (
                        <>
                            <span className="w-4 h-4 border-2 border-white border-t-transparent rounded-full animate-spin mr-2" />
                            Analyzing...
                        </>
                    ) : (
                        "✨ Analyze Workspace"
                    )}
                </Button>
            </div>

            {showSuggestions && suggestions.length > 0 && (
                <SuggestionsCard
                    suggestions={suggestions}
                    onClose={() => setShowSuggestions(false)}
                    onApply={onMerge}
                />
            )}

            {analysisWarnings.length > 0 && (
                <Card className="w-80 shadow-xl pointer-events-auto bg-white/95 backdrop-blur border-amber-100">
                    <CardHeader className="p-3 border-b border-amber-50 bg-gradient-to-r from-amber-50 to-orange-50">
                        <CardTitle className="text-sm font-medium flex items-center gap-2 text-amber-900">
                            ⚠️ Analysis Warnings
                        </CardTitle>
                    </CardHeader>
                    <CardContent className="p-3 max-h-60 overflow-y-auto">
                        <div className="flex flex-col gap-2">
                            {analysisWarnings.map((w, i) => (
                                <div
                                    key={i}
                                    className="text-xs p-2 bg-amber-50 border border-amber-100 rounded text-amber-900"
                                >
                                    <p>{w}</p>
                                </div>
                            ))}
                        </div>
                    </CardContent>
                </Card>
            )}
        </div>
    );
};
