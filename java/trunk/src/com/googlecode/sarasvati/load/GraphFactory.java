/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
package com.googlecode.sarasvati.load;

import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;

public interface GraphFactory<G extends Graph>
{
  /**
   * Generates a new graph instance with the given name and version.
   *
   * @param name The name of the graph to create
   * @param version The version of the graph to create
   * @return A new {@link Graph}
   */
  G newGraph (String name, int version);

  /**
   * Creates a new {@link Arc} with given start node, end node and node name.
   *
   * @param startNode The arc starting node
   * @param endNode   The arc ending node
   * @param name      The arc name
   *
   * @return The new Arc
   *
   * @throws LoadException If the arc can not be created, or if it is passed invalid data.
   */
  Arc newArc (G graph, Node startNode, Node endNode, String name) throws LoadException;

  /**
   * Creates a new {@link Node}.
   *
   * @param graph The graph which the node is part of
   * @param name  The node name
   * @param type  The node type
   * @param isJoin Indicates whether the node is join node
   * @param isStart Indicates whether the node is a start node
   * @param guard  The node guard
   * @param customList A list of custom attributes from the xml file. May be empty or null.
   *
   * @return The new Node
   *
   * @throws LoadException If an error occurs while load, such as incorrect custom data is given.
   */
  Node newNode (G graph, String name, String type, boolean isJoin, boolean isStart, String guard, List<Object> customList)
    throws LoadException;

  /**
   * Imports a node from an external graph into the given graph.
   *
   * @param graph The graph to import the node into
   * @param node  The node to import.
   * @param instanceName The instance name given to the externa graph.
   *
   * @return The new, imported node
   */
  Node importNode (G graph, Node node, String instanceName);

  /**
   * Generates a new {@link GraphProcess} for the given {@link Graph}. Execution
   * of the process is not started by this method.
   *
   * The method will be used by {@link Engine#startProcess(Graph)}.
   *
   * @param graph The {@link Graph} the GraphProcess will be executing.
   * @return A new {@link GraphProcess}
   */
  GraphProcess newProcess (Graph graph);

  /**
   * Generates a new {@link GraphProcess} for the given {@link Graph}. Execution
   * of the process is not started by this method. This method is specfically
   * for created a new nested process.
   *
   * @param graph The {@link Graph} the GraphProcess will be executing.
   * @param parentToken The {@link NodeToken} whose execution is causing this
   *                    nested process to be created.
   * @return A new {@link GraphProcess}
   */
  GraphProcess newNestedProcess (Graph graph, NodeToken parentToken);

  /**
   * Generates a new {@link NodeToken} for the given {@link GraphProcess}, pointing
   * to the given {@link Node}.
   *
   * @param process The {@link GraphProcess} which the new {@link NodeToken} will belong to
   * @param node    The {@link Node} the new {@link NodeToken} is being placed on
   * @param parents The list of ArcTokens which preceded this {@link NodeToken}
   * @return        A new {@link NodeToken}
   */
  NodeToken newNodeToken (GraphProcess process, Node node, List<ArcToken> parents);

  /**
   * Generates a new {@link ArcToken} for the given {@link GraphProcess}, pointing
   * to the given {@link Arc}.
   *
   * @param process The {@link GraphProcess} which the new ArcToken will belong to
   * @param arc     The {@link Arc} the new ArcToken is being placed on
   * @param parent  The {@link NodeToken} which preceded this {@link ArcToken}.
   * @return A new {@link ArcToken}
   */
  ArcToken newArcToken (GraphProcess process, Arc arc, NodeToken parent);

  /**
   * Adds the type to the {@link GraphFactory} for this engine. Specifies
   * what class will be used for a given node type, when loading process
   * definitions from XML file.
   *
   * @param type The type identifier, as used in the process definition file
   * @param nodeClass The node class which will be instantiated for this type
   */
  void addType (String type, Class<? extends Node> nodeClass );

  /**
   * Allows custom loading logic to be used to create nodes of a specific type.
   *
   * @param type The node type, as used in the process definition XML file
   * @param nodeFactory The factory used to create nodes of this type
   */
  void addNodeFactory (String type, NodeFactory nodeFactory);
}