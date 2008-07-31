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
package com.googlecode.sarasvati.visual.jung;

import java.awt.geom.Point2D;

import org.apache.commons.collections15.Transformer;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Node;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.graph.Graph;

public class TreeLayout extends AbstractLayout<Node, Arc>
{
  protected TreeLayout (Graph<Node, Arc> graph)
  {
    super( graph );
  }

  protected TreeLayout (Graph<Node, Arc> graph, Transformer<Node, Point2D> trans)
  {
    super( graph, trans );
  }

  @Override
  public void initialize()
  {
    /* does nothing */
  }

  @Override
  public void reset()
  {
    /* does nothing */
  }
}
