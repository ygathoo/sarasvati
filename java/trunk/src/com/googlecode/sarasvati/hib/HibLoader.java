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

package com.googlecode.sarasvati.hib;

import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.BaseLoader;
import com.googlecode.sarasvati.ImportException;
import com.googlecode.sarasvati.Graph;

public class HibLoader extends BaseLoader<HibGraph, HibNodeRef>
{
  public static interface NodeFactory
  {
    HibNode createNode( HibEngine engine, HibNode node, Object custom)
      throws ImportException;
  }

  protected Map<String,NodeFactory> customTypeFactories = new HashMap<String, NodeFactory>();
  protected HibEngine engine;

  public HibLoader (HibEngine engine)
  {
    this.engine = engine;
  }

  public void addCustomType (String type, NodeFactory factory)
  {
    customTypeFactories.put( type, factory );
  }

  @Override
  protected HibGraph createWfGraph (String name)
  {
    Graph latest = engine.getLatestGraph( name );

    int version = latest == null ? 1 : latest.getVersion() + 1;

    HibGraph newGraph = new HibGraph( name, version );
    engine.getSession().save( newGraph );
    return newGraph;
  }

  @Override
  protected HibNodeRef createNode (String name,
                                   String type,
                                   boolean isJoin,
                                   boolean isStart,
                                   String guard,
                                   Object custom)
    throws ImportException
  {
    HibNode node = new HibNode(getWfGraph(), name, type, isJoin, isStart, guard);

    NodeFactory factory = customTypeFactories.get( type );

    if ( factory == null )
    {
      engine.getSession().save( node );
    }
    else
    {
      node = factory.createNode( engine, node, custom );
    }

    HibNodeRef nodeRef = new HibNodeRef( getWfGraph(), node, "" );
    engine.getSession().save( nodeRef  );
    getWfGraph().getNodeRefs().add( nodeRef );

    return nodeRef;
  }

  @Override
  protected void createArc (HibNodeRef startNode, HibNodeRef endNode, String name)
      throws ImportException
  {
    HibArc arc = new HibArc( getWfGraph(), startNode, endNode, name );
    engine.getSession().save( arc );
    getWfGraph().getArcs().add( arc );
  }

  @Override
  protected Map<String,HibNodeRef> importInstance (String externalName, String instanceName)
      throws ImportException
  {
    HibGraph instanceGraph = engine.getLatestGraph( externalName );

    if ( instanceGraph == null )
    {
      throw new ImportException( "Referenced external '" + externalName + "' not found in database" );
    }

    Map<String, HibNodeRef> refMap = new HashMap<String, HibNodeRef>();
    Map<Long,HibNodeRef>    arcRefMap = new HashMap<Long, HibNodeRef>();

    for ( HibNodeRef nodeRef : instanceGraph.getNodeRefs() )
    {
      String label = nodeRef.getInstance();
      label = label == null || "".equals( label ) ? instanceName : instanceName + ":" + label;

      HibNodeRef newRef = new HibNodeRef( getWfGraph(), nodeRef.getNode(), label );
      engine.getSession().save( newRef );

      arcRefMap.put( nodeRef.getId(), newRef );
      if ( nodeRef.isNodeDefinedInTopLevel() )
      {
        refMap.put( nodeRef.getName(), newRef );
      }
    }

    for ( HibArc arc : instanceGraph.getArcs() )
    {
      HibNodeRef startNode = arcRefMap.get( arc.getStartNode().getId() );
      HibNodeRef endNode = arcRefMap.get( arc.getEndNode().getId() );
      HibArc newArc = new HibArc( getWfGraph(), startNode, endNode, arc.getName() );
      engine.getSession().save( newArc );
    }

    return refMap;
  }

  public boolean isLoaded (String name)
  {
    return null != engine.getLatestGraph( name );
  }
}