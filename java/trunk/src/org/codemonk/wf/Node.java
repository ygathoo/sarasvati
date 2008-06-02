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
 * A node is a vertex in a work-flow graph. It may perform some
 * function. In some cases it may not complete immediately, but
 * enter a wait state. At some point it will return and the
 * process can then continue execution.
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
  String getName ();
  String getType ();
  boolean isJoin ();
  String getGuard ();
  GuardResponse guard (WfEngine engine, NodeToken token);

  /**
   * @param engine The engine which is performing the execution.
   * @param token The token which is currently executing in this node.
   */
  void execute(WfEngine engine, NodeToken token);

  String getLabel ();
}
