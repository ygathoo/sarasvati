package org.codemonk.wf.visual;

import java.awt.Graphics;
import java.util.LinkedList;
import java.util.List;

import org.codemonk.wf.db.NodeRef;

public class GraphTreeNode
{
  protected GraphTreeNode parent;

  protected NodeRef node;
  protected int     depth;
  protected int     index;

  protected List<GraphTreeNode> children = new LinkedList<GraphTreeNode>();
  protected NodePainter painter;

  public GraphTreeNode (GraphTreeNode parent, NodeRef node)
  {
    this.node = node;
    this.parent = parent;

    if ( parent != null )
    {
      this.depth = parent.getDepth() + 1;
      parent.addChild(  this );
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

  public void addChild (GraphTreeNode treeNode)
  {
    children.add( treeNode );
  }

  public Iterable<GraphTreeNode> getChildren ()
  {
    return children;
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
  }

  public int getOriginX ()
  {
    return ((getDepth() + 1) * NodeDrawConfig.getNodeSpacing()) +
           (getDepth() * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
           NodeDrawConfig.getMaxNodeRadius();
  }

  public int getOriginY ()
  {
    return ((getIndex() + 1) * NodeDrawConfig.getNodeSpacing()) +
           (getIndex() * (NodeDrawConfig.getMaxNodeRadius() << 1)) +
           NodeDrawConfig.getMaxNodeRadius();
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