package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JComponent;

import org.codemonk.wf.IArc;
import org.codemonk.wf.db.Graph;
import org.codemonk.wf.db.NodeRef;

public class GraphDrawing extends JComponent
{
  private static final long serialVersionUID = 1L;

  protected Graph graph = null;
  protected GraphTree graphTree = null;

  protected static final Map<String, NodePainter> painterMap = new HashMap<String, NodePainter>();

  static
  {
    painterMap.put( "start", new StartNodePainter() );
  }

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

      int width  = (graphTree.getLayerCount() + 2) * 50;
      int height = (maxHeight + 2) * 50;

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

        NodePainter painter = painterMap.get( treeNode.getNode().getType() );
        if ( painter == null )
        {
          painter = new DefaultNodePainter();
        }

        int left = (x + 1) * 50;
        int top  = (y + 1) * 50;

        painter.paintNode( g, treeNode.getNode(), left, top );

        for ( IArc arc : graph.getOutputArcs( treeNode.getNode() ) )
        {
          GraphTreeNode targetNode = graphTree.getTreeNode( (NodeRef)arc.getEndNode() );
          drawArc( g, arc, treeNode, targetNode );
        }
      }
    }
  }

  protected void drawArc (Graphics g, IArc arc, GraphTreeNode start, GraphTreeNode end)
  {
    if ( "reject".equals( arc.getName() ) )
    {
      g.setColor( Color.red );
    }
    else
    {
      g.setColor( Color.black );
    }

    if ( start.getDepth() < end.getDepth() )
    {
      int startX = (start.getDepth() + 1) * 50 + 5;
      int startY = (start.getIndex() + 1) * 50 + 5;

      int endX = (end.getDepth() + 1) * 50 + 5;
      int endY = (end.getIndex() + 1) * 50 + 5;

      if ( start.getDepth() == end.getDepth() - 1 )
      {
        g.drawLine( startX, startY, endX, endY );
      }
      else
      {
        int midX = startX + 50;
        int midY = end.getIndex() * 50 + 20;
        g.drawLine( startX, startY, midX, midY );
        g.drawLine( midX, midY, endX, endY );
      }
    }
  }
}
