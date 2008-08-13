package com.googlecode.sarasvati;

import java.util.List;

public interface GraphFactory
{
  /**
   * Generates a new graph instance with the given name and version.
   *
   * @param name The name of the graph to create
   * @param version The version of the graph to create
   * @return A new {@link Graph}
   */
  Graph newGraph (String name, int version);

  /**
   * Creates a new {@link Arc} with given start node, end node and node name.
   *
   * @param startNode The arc starting node
   * @param endNode   The arc ending node
   * @param name      The arc name
   *
   * @return The new Arc
   *
   * @throws ImportException If the arc can not be created, or if it is passed invalid data.
   */
  Arc createArc (Graph graph, Node startNode, Node endNode, String name) throws ImportException;

  /**
   * Imports a node from an external graph into the given graph.
   *
   * @param graph The graph to import the node into
   * @param node  The node to import.
   * @param instanceName The instance name given to the externa graph.
   *
   * @return The new, imported node
   */
  Node importNode (Graph graph, Node node, String instanceName);

  /**
   * Generates a new {@link Process} for the given {@link Graph}. Execution
   * of the process is not started by this method.
   *
   * The method will be used by {@link Engine#startProcess(Graph)}, and
   * should not need to be called otherwise.
   *
   * @param graph The {@link Graph} the Process will be executing.
   * @return A new {@link Process}
   */
  Process newProcess (Graph graph);

  /**
   * Generates a new {@link NodeToken} for the given {@link Process}, pointing
   * to the given {@link Node}.
   *
   * @param process The {@link Process} which the new {@link NodeToken} will belong to
   * @param node    The {@link Node} the new {@link NodeToken} is being placed on
   * @param parents The list of ArcTokens which preceded this {@link NodeToken}
   * @return        A new {@link NodeToken}
   */
  NodeToken newNodeToken (Process process, Node node, List<ArcToken> parents);

  /**
   * Generates a new {@link ArcToken} for the given {@link Process}, pointing
   * to the given {@link Arc}.
   *
   * @param process The {@link Process} which the new ArcToken will belong to
   * @param arc     The {@link Arc} the new ArcToken is being placed on
   * @param parent  The {@link NodeToken} which preceded this {@link ArcToken}.
   * @return A new {@link ArcToken}
   */
  ArcToken newArcToken (Process process, Arc arc, NodeToken parent);
}