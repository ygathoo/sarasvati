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

import java.util.HashMap;
import java.util.Map;

import org.codemonk.wf.BaseWfLoader;
import org.codemonk.wf.ImportException;

public class MemWfLoader extends BaseWfLoader<MemWfGraph,MemNode>
{
  public static interface NodeFactory
  {
    MemNode createNode (MemNode node, Object custom)
      throws ImportException;
  }

  protected Map<String,NodeFactory> customTypeFactories = new HashMap<String, NodeFactory>();

  public void addCustomType (String type, NodeFactory factory)
  {
    customTypeFactories.put( type, factory );
  }

  @Override
  protected void createArc (MemNode startNode, MemNode endNode, String name) throws ImportException
  {
    MemArc arc = new MemArc(name, startNode, endNode );
    getWfGraph().getArcs().add( arc );
  }

  @Override
  protected MemNode createNode (String name,
                                String type,
                                boolean isJoin,
                                boolean isStart,
                                String guard,
                                Object custom)
    throws ImportException
  {
    MemNode node = new MemNode( getWfGraph(), name, type, isJoin, isStart, guard );

    NodeFactory factory = customTypeFactories.get( type );

    if ( factory != null )
    {
      node = factory.createNode( node, custom );
    }

    getWfGraph().getNodes().add( node );
    return node;
  }

  @Override
  protected MemWfGraph createWfGraph (String name)
  {
    MemWfGraph graph = new MemWfGraph( name );
    MemWfGraphCache.addToCache( name, graph );
    return graph;
  }

  @Override
  protected Map<String,MemNode> importInstance (String externalName, String instanceName) throws ImportException
  {
    Map<String,MemNode> nodeMap = new HashMap<String, MemNode>();

    MemWfGraph instanceGraph = MemWfGraphCache.get( externalName );

    if ( instanceGraph == null )
    {
      throw new ImportException( "Referenced external '" + externalName + "' not found in cache." );
    }

    Map<MemNode, MemNode> lookupMap = new HashMap<MemNode, MemNode>();

    for ( MemNode node : instanceGraph.getNodes() )
    {
      MemNode newNode = node.clone();
      newNode.setGraph( getWfGraph() );
      newNode.setExternal( true );
      getWfGraph().getNodes().add( newNode );

      lookupMap.put( node, newNode );

      // Since we can only link nodes defined in the top level,
      // we don't want to include nodes which were themselves imported
      // into the graph which is being imported
      if ( !node.isExternal() )
      {
        nodeMap.put( node.getName(), newNode );
      }
    }

    for ( MemArc arc : instanceGraph.getArcs() )
    {
      MemNode startNode = lookupMap.get( arc.getStartNode() );
      MemNode endNode = lookupMap.get( arc.getEndNode() );

      MemArc newArc = new MemArc( arc.getName(), startNode, endNode );
      getWfGraph().getArcs().add( newArc );
    }

    return nodeMap;
  }

  @Override
  public boolean isLoaded (String name)
  {
    return null != MemWfGraphCache.get( name );
  }
}