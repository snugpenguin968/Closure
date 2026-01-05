/**
 * Optimizer Context
 */

"use client";

import React, { createContext, useContext, useEffect, useState } from "react";
import { ManagedRuntime } from "effect";
import { OptimizerService, OptimizerServiceLive } from "./effects";

const ServiceContext = createContext<OptimizerService | null>(null);

const runtime = ManagedRuntime.make(OptimizerServiceLive);

export const OptimizerProvider = ({ children }: { children: React.ReactNode }) => {
  const [service, setService] = useState<OptimizerService | null>(null);

  useEffect(() => {
    runtime.runPromise(OptimizerService).then(setService);
  }, []);

  if (!service) {
    return null;
  }

  return <ServiceContext.Provider value={service}>{children}</ServiceContext.Provider>;
};

export const useOptimizerService = () => {
  const ctx = useContext(ServiceContext);
  if (!ctx) {
    throw new Error("useOptimizerService must be used within OptimizerProvider");
  }
  return ctx;
};
