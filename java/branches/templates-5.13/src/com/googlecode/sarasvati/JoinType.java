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
package com.googlecode.sarasvati;

import com.googlecode.sarasvati.impl.AndJoinStrategy;
import com.googlecode.sarasvati.impl.LabelAndJoinStrategy;
import com.googlecode.sarasvati.impl.OrJoinStrategy;

/**
 * Enumerates the types of joins.
 *
 * @author Paul Lorenz
 */
public enum JoinType
{
  /**
   * Uses the {@link OrJoinStrategy}. A join of this type
   * will be satisfied any time an arc token arrives at the
   * node.
   */
  OR( new OrJoinStrategy() ),

  /**
   * Uses the {@link AndJoinStrategy}. A join of this type
   * will be satisfied when an arc token arrives, and there
   * are arc tokens waiting at all other incoming arcs to
   * the node.
   */
  AND( new AndJoinStrategy() ),

  /**
   * Uses the {@link LabelAndJoinStrategy}. A join of this type
   * will be satisfied when an arc token arrives, and there
   * are arc tokens waiting at all other incoming arcs to the
   * node which share the same name/label as the arc that the
   * arc token is arriving on.
   */
  LABEL_AND( new LabelAndJoinStrategy() );

  private final JoinStrategy joinStrategy;

  private JoinType (final JoinStrategy joinStrategy)
  {
    this.joinStrategy = joinStrategy;
  }

  public JoinStrategy getJoinStrategy ()
  {
    return joinStrategy;
  }
}