/**
 * Workspace Context
 * Provides the WorkspaceService to the component tree.
 */

"use client";

import React, { createContext, useContext, useMemo } from "react";
import { ManagedRuntime } from "effect";
import { WorkspaceService, WorkspaceServiceLive } from "./effects";

const ServiceContext = createContext<WorkspaceService>({} as unknown as WorkspaceService);

export const WorkspaceProvider = ({ children }: { children: React.ReactNode }) => {
  // Synchronously create runtime and get service (since we are client-side only
  // and using runSync for the service creation effect is safe if it doesn't do async IO in init)
  // Actually, `make` just does Ref.make and PubSub.make which are synchronous.
  // So we can use runSync.

  const service = useMemo(() => {
    const runtime = ManagedRuntime.make(WorkspaceServiceLive);
    return runtime.runSync(WorkspaceService);
  }, []);

  return <ServiceContext.Provider value={service}>{children}</ServiceContext.Provider>;
};

export const useWorkspaceService = () => useContext(ServiceContext);
