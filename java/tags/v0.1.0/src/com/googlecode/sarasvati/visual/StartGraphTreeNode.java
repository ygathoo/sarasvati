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
import java.awt.Graphics;

import com.googlecode.sarasvati.Node;

public class StartGraphTreeNode extends GraphTreeNode
{
  public StartGraphTreeNode( GraphTreeNode parent, Node node )
  {
    super( parent, node );
    color = new Color( 102, 152, 102 );
  }

  @Override
  public void paintNode( Graphics g )
  {
    g.setColor( color );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();
    int offset = getOffset();
    g.fillOval( originX - offset, originY - offset, maxRadius, maxRadius);

    offset = maxRadius >> 2;

    int xd = originX + (maxRadius / 10);
    g.setColor( Color.white );
    g.fillPolygon( new int[] { xd - offset,       xd + offset, xd - offset },
                   new int[] { originY  - offset, originY,     originY  + offset },
                   3 );
  }

  @Override
  public int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius() >> 1;
  }
}
