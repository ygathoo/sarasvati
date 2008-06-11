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

import org.codemonk.wf.Arc;
import org.codemonk.wf.Node;
import org.codemonk.wf.WfGraph;

public class MemWfGraph implements WfGraph
{
  protected String        name;
  protected List<MemNode> nodes;
  protected List<MemArc>  arcs;

  public MemWfGraph (String name)
  {
    this.name  = name;
    this.nodes = new LinkedList<MemNode>();
    this.arcs  = new LinkedList<MemArc>();
  }

  public List<MemNode> getNodes ()
  {
    return nodes;
  }

  public List<MemArc> getArcs ()
  {
    return arcs;
  }

  @Override
  public List<Arc> getInputArcs (Node node)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<Arc> getInputArcs (Node node, String arcName)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  @Override
  public List<Arc> getOutputArcs (Node node)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<Arc> getOutputArcs (Node node, String arcName)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<Node> getStartNodes ()
  {
    List<Node> startNodes = new LinkedList<Node>();

    for ( Node node : getNodes() )
    {
      if ( node.isStart() )
      {
        startNodes.add( node );
      }
    }

    return startNodes;
  }

  @Override
  public int getVersion ()
  {
    return 1;
  }
}
