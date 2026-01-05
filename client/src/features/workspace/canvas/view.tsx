"use client";

import React from "react";
import ReactFlow, {
  type Node,
  type Edge,
  type NodeChange,
  type EdgeChange,
  Background,
  Controls,
  type NodeTypes,
  type Connection,
} from "reactflow";
import "reactflow/dist/style.css";
import { TableNode } from "./components/TableNode";

export interface CanvasViewProps {
  nodes: Node[];
  edges: Edge[];
  onNodesChange: (changes: NodeChange[]) => void;
  onEdgesChange: (changes: EdgeChange[]) => void;
  onConnect: (connection: Connection) => void;
  onNodeDragStop: (event: React.MouseEvent, node: Node) => void;
  onEdgeClick?: (event: React.MouseEvent, edge: Edge) => void;
}

const nodeTypes: NodeTypes = {
  table: TableNode,
};

export const CanvasView: React.FC<CanvasViewProps> = ({
  nodes,
  edges,
  onNodesChange,
  onEdgesChange,
  onConnect,
  onNodeDragStop,
  onEdgeClick,
}): React.ReactElement => {
  return (
    <div className="w-full h-full bg-gradient-to-br from-slate-50 to-slate-100 relative">
      <ReactFlow
        nodes={nodes}
        edges={edges}
        onNodesChange={onNodesChange}
        onEdgesChange={onEdgesChange}
        onConnect={onConnect}
        onNodeDragStop={onNodeDragStop}
        onEdgeClick={onEdgeClick}
        nodeTypes={nodeTypes}
        defaultViewport={{ x: 100, y: 100, zoom: 1 }}
        minZoom={0.1}
        maxZoom={2}
        deleteKeyCode={["Backspace", "Delete"]}
        proOptions={{ hideAttribution: true }}
        className="bg-transparent"
      >
        <Background color="#cbd5e1" gap={20} size={1} />
        <Controls className="bg-white border-slate-200 shadow-lg rounded-lg" />
      </ReactFlow>
    </div>
  );
};
