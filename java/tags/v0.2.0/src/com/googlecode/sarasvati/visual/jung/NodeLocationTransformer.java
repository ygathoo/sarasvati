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
/**
 * Created on May 15, 2008
 */
package com.googlecode.sarasvati.visual.jung;

import java.awt.geom.Point2D;

import org.apache.commons.collections15.Transformer;

import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.visual.GraphTree;
import com.googlecode.sarasvati.visual.GraphTreeNode;

public class NodeLocationTransformer implements Transformer<Node, Point2D>
{
  protected GraphTree graphTree;

  public NodeLocationTransformer (GraphTree graphTree)
  {
    this.graphTree = graphTree;
  }

  @Override
  public Point2D transform (Node node)
  {
    GraphTreeNode treeNode = graphTree.getTreeNode( node );
    Point2D point = new Point2D.Double();
    point.setLocation( treeNode.getDepth() * 100 + 50, treeNode.getIndex() * 100 + 50 );
    return point;
  }
}
