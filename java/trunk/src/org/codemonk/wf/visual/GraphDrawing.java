package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;
import java.util.List;

import javax.swing.JComponent;

import org.codemonk.wf.db.Graph;

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
        int left = (x + 1) * 50;
        int top  = (y + 1) * 50;
        g.fillRect( left, top, 10, 10 );
      }
    }
  }
}
