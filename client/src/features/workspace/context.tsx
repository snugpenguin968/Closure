/**
 * Workspace Context
 * Provides the WorkspaceService to the component tree.
 */

"use client";

import React, { createContext, useContext, useEffect, useState } from 'react';
import { Effect, Layer, ManagedRuntime } from "effect";
import { WorkspaceService, WorkspaceServiceLive } from "./effects";

const ServiceContext = createContext<WorkspaceService | null>(null);

// Create a ManagedRuntime for the service layer
// This ensures the service lives as long as the runtime (app lifetime)
const runtime = ManagedRuntime.make(WorkspaceServiceLive);

export const WorkspaceProvider = ({ children }: { children: React.ReactNode }) => {
    const [service, setService] = useState<WorkspaceService | null>(null);

    useEffect(() => {
        // Extract the service from the runtime
        runtime.runPromise(WorkspaceService).then(setService);
    }, []);

    if (!service) return <div>Loading Workspace...</div>;

    return (
        <ServiceContext.Provider value={service}>
            {children}
        </ServiceContext.Provider>
    );
};

export const useWorkspaceService = () => {
    const ctx = useContext(ServiceContext);
    if (!ctx) throw new Error("useWorkspaceService must be used within WorkspaceProvider");
    return ctx;
};
