package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.util.List;

import javax.swing.JComponent;

import org.codemonk.wf.IArc;
import org.codemonk.wf.db.Graph;
import org.codemonk.wf.db.NodeRef;

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
      Point startPoint = start.getRightAnchor();
      Point endPoint   = end.getLeftAnchor();

      if ( start.getDepth() == end.getDepth() - 1 )
      {
        g.drawLine( startPoint.x, startPoint.y, endPoint.x, endPoint.y );
      }
    }
  }

  @Override
  public Dimension getPreferredSize ()
  {
    return getSize();
  }
}
