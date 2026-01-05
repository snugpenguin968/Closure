"use client";

import React from "react";
import { CanvasAdapter } from "./canvas/adapter";
import { DashboardAdapter } from "./dashboard/adapter";

export const WorkspaceAdapter = (): React.ReactElement => {
  return (
    <div className="w-full h-screen relative">
      <CanvasAdapter />
      <DashboardAdapter />
    </div>
  );
};
