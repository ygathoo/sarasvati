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
package com.googlecode.sarasvati.visual.graph;

import java.awt.Component;
import java.awt.Point;

import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.visual.GraphSceneImpl;

public class SarasvatiGraphScene extends GraphSceneImpl<Node, Arc>
{
  protected Graph graph;
  protected GraphTree graphTree;

  public SarasvatiGraphScene (Graph graph)
  {
    if ( graph != null )
    {
      for ( Node ref : graph.getNodes() )
      {
        addNode( ref );
      }

      for ( Arc arc : graph.getArcs() )
      {
        addEdge( arc );
        setEdgeSource( arc, arc.getStartNode() );
        setEdgeTarget( arc, arc.getEndNode() );
      }

      graphTree = new GraphTree( graph );

      for ( Node node : graph.getNodes() )
      {
        Widget widget = findWidget( node );
        GraphTreeNode treeNode = graphTree.getTreeNode( node );
        widget.setPreferredLocation( new Point( treeNode.getOriginX(), treeNode.getOriginY() ) );
        widget.resolveBounds( new Point( treeNode.getOriginX(), treeNode.getOriginY() ), null );
      }

      revalidate();
    }
  }

  public Graph getGraph ()
  {
    return graph;
  }

  public GraphTree getGraphTree ()
  {
    return graphTree;
  }

  @Override
  protected Widget widgetForNode (Node node)
  {
    Component c = node.getAdaptor( Component.class );

    if ( c == null )
    {
      throw new IllegalArgumentException( "No component found for node: " + node +
                                          ". You should configure one with NodeAdapterManager.registerFactory " +
                                          "and by subclassing your Node implemenations." );
    }

    return new ComponentWidget( this, c );
  }
}