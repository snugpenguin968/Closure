"use client";

import { WorkspaceProvider } from "@/features/workspace/context";
import { OptimizerProvider } from "@/features/optimizer/context";
import { WorkspaceAdapter } from "@/features/workspace/adapter";
import { OptimizerAdapter } from "@/features/optimizer/adapter";

export default function Home() {
  return (
    <main className="w-full h-screen overflow-hidden bg-slate-50">
      <WorkspaceProvider>
        <OptimizerProvider>
          {/* Main Canvas */}
          <WorkspaceAdapter />

          {/* Overlay Features */}
          <OptimizerAdapter />

        </OptimizerProvider>
      </WorkspaceProvider>
    </main>
  );
}
