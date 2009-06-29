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

    Copyright 2009 Paul Lorenz
*/

package com.googlecode.sarasvati.editor.model;

import com.googlecode.sarasvati.JoinType;

public class NodeState extends GraphMemberState
{
  private final String  type;
  private final JoinType joinType;
  private final String joinParam;
  private final boolean isStart;
  private final String  guard;

  public NodeState (final String name,
                    final String type,
                    final JoinType joinType,
                    final String joinParam,
                    final boolean isStart,
                    final String guard)
  {
    super( name );
    this.type = type;
    this.joinType = joinType;
    this.joinParam = joinParam;
    this.isStart = isStart;
    this.guard = guard;
  }

  public String getType ()
  {
    return type;
  }

  public JoinType getJoinType ()
  {
    return joinType;
  }

  public String getJoinParam ()
  {
    return joinParam;
  }

  public boolean isStart ()
  {
    return isStart;
  }

  public String getGuard ()
  {
    return guard;
  }
}
