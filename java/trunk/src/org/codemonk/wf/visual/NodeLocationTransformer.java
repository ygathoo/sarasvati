/**
 * Created on May 15, 2008
 */
package org.codemonk.wf.visual;

import java.awt.geom.Point2D;

import org.apache.commons.collections15.Transformer;
import org.codemonk.wf.db.HibNodeRef;

public class NodeLocationTransformer implements Transformer<HibNodeRef, Point2D>
{
  protected GraphTree graphTree;

  public NodeLocationTransformer (GraphTree graphTree)
  {
    this.graphTree = graphTree;
  }

  @Override
  public Point2D transform (HibNodeRef nodeRef)
  {
    GraphTreeNode treeNode = graphTree.getTreeNode( nodeRef );
    Point2D point = new Point2D.Double();
    point.setLocation( treeNode.getDepth() * 100 + 50, treeNode.getIndex() * 100 + 50 );
    return point;
  }
}
