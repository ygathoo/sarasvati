package org.codemonk.wf.visual;

import java.awt.Graphics;
import java.util.List;

import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.visual.painter.NodePainter;
import org.codemonk.wf.visual.painter.NodePainterFactory;

public class GraphTreeNode
{
  protected GraphTreeNode parent;

  protected NodeRef node;
  protected int     depth;
  protected int     index;

  protected int     originX;
  protected int     originY;

  protected NodePainter painter;

  public GraphTreeNode (GraphTreeNode parent, NodeRef node)
  {
    this.node = node;
    this.parent = parent;

    if ( parent != null )
    {
      this.depth = parent.getDepth() + 1;
      this.painter = NodePainterFactory.getInstance( node.getType() );
    }
    else
    {
      depth = -1;
    }
  }

  public NodeRef getNode ()
  {
    return node;
  }

  public int getDepth ()
  {
    return depth;
  }

  public int getIndex()
  {
    return index;
  }

  public void setIndex( int index )
  {
    this.index = index;
  }

  public void addToLayer (List<GraphTreeNode> layer)
  {
    this.index = layer.size();
    layer.add( this );
    recalculateOrigin();
  }

  public void recalculateOrigin ()
  {
    originX = ((getDepth() + 1) * NodeDrawConfig.getNodeSpacing()) +
              (getDepth() * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
              NodeDrawConfig.getMaxNodeRadius();

    originY = ((getIndex() + 1) * NodeDrawConfig.getNodeSpacing()) +
              (getIndex() * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
              NodeDrawConfig.getMaxNodeRadius();
  }

  public int getOriginX ()
  {
    return originX;
  }

  public int getOriginY ()
  {
    return originY;
  }

  public void paintNode (Graphics g)
  {
    painter.paintNode( g, node, getOriginX(), getOriginY() );
  }

  public Point getLeftAnchor ()
  {
    return painter.getLeftAnchor( getOriginX(), getOriginY() );
  }

  public Point getRightAnchor ()
  {
    return painter.getRightAnchor( getOriginX(), getOriginY() );
  }

  public Point getTopAnchor ()
  {
    return painter.getTopAnchor( getOriginX(), getOriginY() );
  }
}