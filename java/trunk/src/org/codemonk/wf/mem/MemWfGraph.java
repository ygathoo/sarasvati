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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.codemonk.wf.Arc;
import org.codemonk.wf.Node;
import org.codemonk.wf.WfGraph;

public class MemWfGraph implements WfGraph
{
  protected String        name;
  protected List<MemNode> nodes;
  protected List<MemArc>  arcs;

  protected Map<Node, List<Arc>> inputMap;
  protected Map<Node, List<Arc>> outputMap;

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
  public String getName ()
  {
    return name;
  }

  @Override
  public List<Arc> getInputArcs (Node node)
  {
    if ( inputMap == null )
    {
      initialize();
    }
    return inputMap.get( node );
  }

  @Override
  public List<Arc> getInputArcs (Node node, String arcName)
  {
    List<Arc> arcList = getInputArcs( node );
    List<Arc> result = new ArrayList<Arc>( arcList.size() );

    for ( Arc arc : arcList )
    {
      if ( arcName.equals( arc.getName() ) )
      {
        result.add( arc );
      }
    }
    return result;
  }

  @Override
  public List<Arc> getOutputArcs (Node node)
  {
    if (outputMap == null)
    {
      initialize();
    }
    return outputMap.get( node );
  }

  @Override
  public List<Arc> getOutputArcs (Node node, String arcName)
  {
    List<Arc> arcList = getOutputArcs( node );
    List<Arc> result = new ArrayList<Arc>( arcList.size() );

    for ( Arc arc : arcList )
    {
      if ( arcName.equals( arc.getName() ) )
      {
        result.add( arc );
      }
    }
    return result;
  }

  public void initialize ()
  {
    inputMap  = new HashMap<Node, List<Arc>>();
    outputMap = new HashMap<Node, List<Arc>>();

    for ( Arc arc : arcs )
    {
      Node node = arc.getStartNode();
      List<Arc> list = outputMap.get( node );

      if ( list == null )
      {
        list = new LinkedList<Arc>();
        outputMap.put( node, list );
      }

      list.add( arc );

      node = arc.getEndNode();
      list = inputMap.get( node );

      if ( list == null )
      {
        list = new LinkedList<Arc>();
        inputMap.put( node, list );
      }

      list.add( arc );
    }

    List<Arc> emptyList = Collections.emptyList();
    for (MemNode node : nodes )
    {
      if ( !inputMap.containsKey( node ) )
      {
        inputMap.put( node, emptyList );
      }
      if ( !outputMap.containsKey( node ) )
      {
        outputMap.put( node, emptyList );
      }
    }
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
