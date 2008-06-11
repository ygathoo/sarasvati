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

package org.codemonk.wf.mem;

import org.codemonk.wf.Arc;
import org.codemonk.wf.GuardResponse;
import org.codemonk.wf.Node;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.WfEngine;

public class MemNode implements Node
{
  protected MemWfGraph graph;

  protected String  name;
  protected String  type;
  protected boolean isJoin;
  protected boolean isStart;
  protected String guard;

  protected boolean isExternal = false;

  public MemNode (MemWfGraph graph, String name, String type, boolean isJoin, boolean isStart, String guard)
  {
    this.graph = graph;
    this.name = name;
    this.type = type;
    this.isJoin = isJoin;
    this.isStart = isStart;
    this.guard = guard;
  }

  public MemNode (MemNode node)
  {
    this.graph = node.getGraph ();
    this.name = node.getName();
    this.type = node.getType();
    this.isJoin = node.isJoin();
    this.isStart = node.isStart();
    this.guard = node.getGuard();
  }

  @Override
  public void execute (WfEngine engine, NodeToken token)
  {
    engine.completeExecuteNode( token, Arc.DEFAULT_ARC );
  }

  public MemWfGraph getGraph ()
  {
    return graph;
  }

  @Override
  public String getGuard ()
  {
    return guard;
  }

  @Override
  public String getLabel ()
  {
    return name;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  @Override
  public String getType ()
  {
    return type;
  }

  @Override
  public GuardResponse guard (WfEngine engine, NodeToken token)
  {
    return GuardResponse.ACCEPT_TOKEN_RESPONSE;
  }

  @Override
  public boolean isJoin ()
  {
    return isJoin;
  }

  @Override
  public boolean isStart ()
  {
    return isStart && !isExternal;
  }

  public boolean isExternal ()
  {
    return isExternal;
  }

  public void setExternal (boolean isExternal)
  {
    this.isExternal = isExternal;
  }
}