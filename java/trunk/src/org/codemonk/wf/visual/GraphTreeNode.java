package org.codemonk.wf.visual;

import java.util.LinkedList;
import java.util.List;

import org.codemonk.wf.db.NodeRef;

public class GraphTreeNode
{
  protected GraphTreeNode parent;

  protected NodeRef node;
  protected int     depth;
  protected int     index = 0;

  protected List<GraphTreeNode> children = new LinkedList<GraphTreeNode>();

  public GraphTreeNode (GraphTreeNode parent, NodeRef node)
  {
    this.node = node;
    this.parent = parent;

    if ( parent != null )
    {
      this.depth = parent.getDepth() + 1;
      parent.addChild(  this );
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
}
