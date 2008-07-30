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
package com.googlecode.sarasvati;

/**
 * Arc tokens point to arcs in the graph. Arc tokens may be active
 * only briefly, if the node at the end of the arc is ready to
 * execute. However, if the node is a join node, and not all incoming
 * arcs have tokens, an arc token may be active for some time.
 *
 * @author Paul Lorenz
 */
public interface ArcToken extends Token
{
  /**
   * Returns the arc that this arc token points to.
   *
   * @return The associated arc
   */
  Arc getArc ();

  /**
   * Returns the process that this node token belongs to.
   *
   * @return The associated process
   */
  Process getProcess ();

  /**
   * Returns the node token which directly preceded this
   * arc token.
   *
   * @return The parent NodeToken
   */
  NodeToken getParentToken ();
}
