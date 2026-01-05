# Closure 💠

**Closure** is a visual database design tool that guarantees your schema is correct. It runs normalization algorithms (BCNF/3NF) on your design in real-time to eliminate redundancy and data anomalies.

## ✨ Why Closure?

Stop guessing if your database is normalized. Define your tables and rules (Functional Dependencies), and Closure will **analyze** your schema to detect redundancy and normalization violations (BCNF/3NF).

*   **Visual Design**: Drag-and-drop tables and define relationships.
*   **Auto-Normalization**: One-click optimization to BCNF or 3NF.
*   **SQL Export**: Generate production-ready SQL schemas.

## 🏗️ Architecture

Closure consists of two high-performance components:

### 1. Client (`/client`)
*   **Framework**: Next.js 14 + React (TypeScript).
*   **State Management**: Effect-TS (Functional Reactive Programming).
*   **Canvas**: React Flow.
*   **Design**: Hierarchical MVA (Model-View-Adapter).
    *   `canvas/`: Handles the graph, nodes, and drag interactions.
    *   `dashboard/`: Handles the HUD, suggestions, and analysis feedback.

### 2. Server (`/server`)
*   **Language**: Haskell 𝜆.
*   **Role**: The "Brain". Handles heavy computational logic:
    *   Calculating Closures of Functional Dependencies.
    *   Bernstein's Synthesis / BCNF Decomposition.
    *   Cycle Detection & Lossless Join verification.
*   **Build Tool**: Stack / Cabal.

---

## 🚀 Getting Started

### Prerequisites
*   **Node.js** (v18+)
*   **Haskell Stack** (for the server) _OR_ **Cabal**.

### 1. Start the Server (Haskell)
The server runs the normalization algorithms.

```bash
cd server
# Build and run with Stack (Recommended)
stack build
stack run

# The server typically runs on localhost:8080 or port 3000 (check console output)
```

### 2. Start the Client (React)
The client provides the visual interface.

```bash
cd client
npm install
npm run dev

# Open http://localhost:3000 to use Closure.
```

---

## 🎮 How to Use

1.  **Draft Your Schema**:
    *   Use the **Canvas** to create tables.
    *   Add **Attributes** (columns) and **Functional Dependencies (FDs)** (rules like `zip_code -> city`).
2.  **Analyze**:
    *   Click **"Analyze Workspace"** in the Dashboard.
    *   Closure sends your schema to the Haskell engine.
    *   It returns:
        *   **Health Score**: Identifying BCNF/3NF violations.
        *   **Suggestions**: Intelligent merges or splits.
        *   **Warnings**: Potential anomalies.
3.  **Optimize**:
    *   Select a strategy:
        *   **BCNF (Strict)**: Maximum consistency, no redundancy. Best for write-heavy apps.
        *   **3NF (Balanced)**: Standard industry target. allows minor redundancy for convenience.
        *   **Performance (Speed)**: Prioritizes **Read Speed** (denormalization). Allows redundancy to reduce JOINs, ideal for analytics or high-traffic read APIs.
    *   Let Closure automatically decompose your tables based on your choice.
4.  **Export**:
    *   Export the result as production-ready **SQL**.


