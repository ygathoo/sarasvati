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
package com.googlecode.sarasvati.visual;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.util.List;

import javax.swing.JComponent;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.hib.HibNodeRef;

public class GraphDrawing extends JComponent
{
  private static final long serialVersionUID = 1L;

  protected Graph graph = null;
  protected GraphTree graphTree = null;

  public GraphDrawing ()
  {
  }

  public Graph getGraph ()
  {
    return graph;
  }

  public void setGraph (Graph graph)
  {
    this.graph = graph;
    this.graphTree = graph == null ? null : new GraphTree( graph );

    if ( graphTree != null )
    {
      int maxHeight = 0;

      for ( List<?> layer : graphTree.getLayers() )
      {
        if ( layer.size() > maxHeight )
        {
          maxHeight = layer.size();
        }
      }

      int nodeSize = NodeDrawConfig.getMaxNodeRadius() << 1;
      int spaceSize = NodeDrawConfig.getNodeSpacing();

      int width  = graphTree.getLayerCount() * nodeSize + (graphTree.getLayerCount() + 1) * spaceSize;

      int height = maxHeight * nodeSize + (maxHeight + 1) * spaceSize;

      setSize( width, height );
    }
  }

  @Override
  public void paintComponent( Graphics g )
  {
    super.paintComponent( g );

    if ( graphTree == null || graphTree.getLayerCount() == 0)
    {
      return;
    }

    g.setColor( Color.white );
    g.fillRect( 0, 0, getWidth(), getHeight() );
    g.setColor( Color.black );

    for ( int x = 0; x < graphTree.getLayerCount(); x++ )
    {
      List<GraphTreeNode> layer = graphTree.getLayer( x );

      for ( int y = 0; y < layer.size(); y++ )
      {
        GraphTreeNode treeNode = layer.get( y );
        treeNode.paintNode( g );

        for ( Arc arc : graph.getOutputArcs( treeNode.getNode() ) )
        {
          GraphTreeNode targetNode = graphTree.getTreeNode( (HibNodeRef)arc.getEndNode() );
          drawArc( g, arc, treeNode, targetNode );
        }
      }
    }
  }

  /**
   *   --------------
   *  /              \
   * ***      ***    ***
   * * * ---> * * -> * *
   * *** \    ***    ***
   *      \
   *       \  ***
   *        > * *
   *          ***
   * @param g
   * @param arc
   * @param start
   * @param end
   */
  protected void drawArc (Graphics g, Arc arc, GraphTreeNode start, GraphTreeNode end)
  {
    boolean isReject = "reject".equals( arc.getName() );
    g.setColor( isReject ? Color.red : Color.black );

    if ( end.getDepth() - start.getDepth() == 1 )
    {
      Point startPoint = start.getRightAnchor();
      Point endPoint   = end.getLeftAnchor();

      /*
      if ( graph.hasArcInverse( arc ) )
      {
        g.drawArc( startPoint.x, startPoint.y, 1,1,1,1 );
      }
      else
      {
      */
        g.drawLine( startPoint.x, startPoint.y, endPoint.x, endPoint.y );
        end.paintLeftIncomingAnchor( g );
      //}
    }
  }

  @Override
  public Dimension getPreferredSize ()
  {
    return getSize();
  }
}
