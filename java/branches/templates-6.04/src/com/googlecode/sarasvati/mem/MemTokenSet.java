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

package com.googlecode.sarasvati.mem;

import java.util.LinkedList;
import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.TokenSet;

public class MemTokenSet implements TokenSet
{
  protected final GraphProcess process;
  protected final String name;

  protected boolean complete = false;

  protected List<ArcToken> activeArcTokens = new LinkedList<ArcToken>();
  protected List<NodeToken> activeNodeTokens = new LinkedList<NodeToken>();

  public MemTokenSet (final GraphProcess process, final String name)
  {
    this.process = process;
    this.name = name;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  @Override
  public GraphProcess getProcess ()
  {
    return process;
  }

  @Override
  public List<ArcToken> getActiveArcTokens (Engine engine)
  {
    return activeArcTokens;
  }

  @Override
  public List<NodeToken> getActiveNodeTokens (Engine engine)
  {
    return activeNodeTokens;
  }

  @Override
  public boolean isComplete ()
  {
    return complete;
  }

  @Override
  public void markComplete (Engine engine)
  {
    complete = true;
  }
}