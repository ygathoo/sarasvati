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

import java.awt.Point;

import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.visual.NodeWidgetFactory;
import com.googlecode.sarasvati.visual.common.GraphSceneImpl;

public class SarasvatiGraphScene extends GraphSceneImpl<Node, Arc>
{
  protected Graph graph;
  protected NodeWidgetFactory widgetFactory;
  protected GraphTree graphTree;
  protected boolean showSelfArcs;

  public SarasvatiGraphScene (Graph graph, NodeWidgetFactory widgetFactory)
  {
    this( graph, widgetFactory, false );
  }

  public SarasvatiGraphScene (Graph graph, NodeWidgetFactory widgetFactory, boolean showSelfArcs)
  {
    this.widgetFactory = widgetFactory;
    this.showSelfArcs = showSelfArcs;

    if ( graph != null )
    {
      for ( Node ref : graph.getNodes() )
      {
        addNode( ref );
      }

      for ( Arc arc : graph.getArcs() )
      {
        if ( showSelfArcs || !arc.getStartNode().equals( arc.getEndNode() ) )
        {
          addEdge( arc );
          setEdgeSource( arc, arc.getStartNode() );
          setEdgeTarget( arc, arc.getEndNode() );
        }
      }

      graphTree = new GraphTree( graph );

      for ( Node node : graph.getNodes() )
      {
        Widget widget = findWidget( node );
        GraphTreeNode treeNode = graphTree.getTreeNode( node );
        Point origin = new Point( treeNode.getOriginX(), treeNode.getOriginY() );
        widget.setPreferredLocation( origin );
        widget.resolveBounds( origin, null );
      }

      for ( Arc arc : graph.getArcs() )
      {
        Widget widget = findWidget( arc );
        if ( widget != null )
        {
          widget.resolveBounds( null, null );
        }
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
    return widgetFactory.newWidget( node, this );
  }
}