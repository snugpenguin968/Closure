/**
 * Configuration for the workspace feature.
 * Uses environment variables with sensible defaults.
 */

// API Configuration
export const API_BASE_URL = process.env.NEXT_PUBLIC_API_URL ?? "http://localhost:8080";
export const API_TIMEOUT = parseInt(process.env.NEXT_PUBLIC_API_TIMEOUT ?? "10000", 10);

// Layout Configuration
export const LAYOUT_HORIZONTAL_SPACING = 350;
export const LAYOUT_VERTICAL_SPACING = 200;

// History Configuration
export const MAX_HISTORY_SIZE = 50;
