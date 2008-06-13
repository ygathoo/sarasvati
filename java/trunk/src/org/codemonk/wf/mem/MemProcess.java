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

import java.util.LinkedList;
import java.util.List;

import org.codemonk.wf.ArcToken;
import org.codemonk.wf.WfGraph;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.Process;

public class MemProcess implements Process
{
  protected long tokenCounter = 0;

  protected WfGraph graph;
  protected List<ArcToken> arcTokens = new LinkedList<ArcToken>();
  protected List<NodeToken> nodeTokens = new LinkedList<NodeToken>();

  public MemProcess( WfGraph graph )
  {
    this.graph = graph;
  }

  @Override
  public void addArcToken (ArcToken token)
  {
    arcTokens.add( token );
  }

  @Override
  public void addNodeToken (NodeToken token)
  {
    nodeTokens.add( token );
  }

  @Override
  public List<? extends ArcToken> getArcTokens ()
  {
    return arcTokens;
  }

  @Override
  public WfGraph getGraph ()
  {
    return graph;
  }

  @Override
  public void removeArcToken (ArcToken token)
  {
    arcTokens.remove( token );
  }

  @Override
  public void removeNodeToken (NodeToken token)
  {
    nodeTokens.remove( token );
  }

  @Override
  public boolean isComplete ()
  {
    return arcTokens.isEmpty() && nodeTokens.isEmpty();
  }

  public synchronized long nextTokenId ()
  {
    return tokenCounter++;
  }
}