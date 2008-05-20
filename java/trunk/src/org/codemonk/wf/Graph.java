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
/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.List;

public interface Graph
{
  /**
   * Returns the graph name, which should be unique. Different versions with the same
   * name may exist, but each of these should have a unique version number.
   *
   * @return The name of the graph
   */
  String getName ();

  /**
   * Returns the version number of the graph. Multiple versions of a graph may exists.
   * Newer versions of the graph should have higher version numbers than older versions.
   * A graph can be uniquely identified by name and version number.
   *
   * @return The version of the graph
   */
  int getVersion ();

  /**
   * Returns a list of arcs which have the given node as an end point. The
   * list may be empty, but will never be null.
   *
   * @param node A node belonging to this graph
   * @return A list of arcs
   */
  List<Arc> getInputArcs (Node node);

  /**
   * Returns a list of arcs which have the given node as an end point and
   * which have the given name. The list may be empty but will never be
   * null.
   *
   * @param node A node belonging to this graph
   * @return A list of arcs
   */
  List<Arc> getInputArcs (Node node, String arcName);

  /**
   * Returns a list of arcs which have the given node as a starting point.
   * The list may be empty but will never be null.
   *
   * @param node A node belonging to this graph
   * @return A list of arcs
   */
  List<Arc> getOutputArcs (Node node);

  /**
   * Returns a list of arcs which have the given node as a starting point
   * and which have the given name. The list may be empty but will never
   * be null.
   *
   * @param node A node belonging to this graph
   * @return A list of arcs
   */
  List<Arc> getOutputArcs (Node node, String arcName);

  /**
   * Returns a list of the nodes at which nodes should be placed to
   * start a workflow process.
   *
   * @return A list of nodes
   */
  List<Node> getStartNodes ();

  /**
   * Returns true if the given arc has an inverse. An inverse arc
   * would have the end node of the given arc as a start node and
   * the start node of the given arc as an end node.
   *
   * @param arc An arc belonging to this graph
   * @return True if an inverse of this arc exists in the graph
   */
  boolean hasArcInverse (Arc arc);
}