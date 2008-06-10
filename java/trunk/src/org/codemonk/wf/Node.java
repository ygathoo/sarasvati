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

/**
 * A node corresponds to an action in a process definition.
 * It may perform some function. In some cases it may not
 * complete immediately, but enter a wait state. At some
 * point it will return and the process can then continue execution.
 *
 * Every node can have a guard associated with it. This guard
 * will determine if an incoming token is accepted (and the
 * node functionality executed), or discarded, or if the token
 * is passed through without executing the node.
 *
 * Every node may have multiple incoming arcs. If isJoin()
 * return true, then tokens coming in will wait for tokens
 * to be present on all arcs with the same label.
 *
 * Every node may also have multiple outgoing arcs. When
 * a node token is completed, it may pick which arcs to exit
 * on by passing an arc label. Every arc with the given label
 * will have an arc token places on it.
 *
 * @author Paul Lorenz
 */
public interface Node
{
  /**
   * Returns the node name. Every node must have a name which
   * is unique in it's process definition;
   *
   * @return The node name.
   */
  String getName ();

  String getType ();

  /**
   * Return true if the node is a join node. When an arc token
   * arrives at a join node, it will wait for arc tokens to be
   * present all other arcs that have the same name before
   * executing the node.
   *
   * When an arc token arrives at a non-join node it executes
   * the node right away.
   *
   * @return True if the node is a join node
   */
  boolean isJoin ();

  /**
   * Returns true if this node is a start node. Start nodes
   * will have a token placed in them when the process is
   * started
   *
   * @return True if the node is a start node.
   */
  boolean isStart ();

  String getGuard ();
  GuardResponse guard (WfEngine engine, NodeToken token);

  /**
   * @param engine The engine which is performing the execution.
   * @param token The token which is currently executing in this node.
   */
  void execute(WfEngine engine, NodeToken token);

  /**
   * Returns the label to be used when the node is being visualized.
   *
   * @return The display label
   */
  String getLabel ();
}
