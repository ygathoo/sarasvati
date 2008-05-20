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
package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Graphics;

import org.codemonk.wf.db.HibNodeRef;
import org.codemonk.wf.test.NodeTask;

public class TaskGraphTreeNode extends GraphTreeNode
{
  public TaskGraphTreeNode( GraphTreeNode parent, HibNodeRef node )
  {
    super( parent, node );
    color = new Color( 102, 152, 102 );
  }

  @Override
  public void paintNode( Graphics g )
  {
    g.setColor( Color.blue );

    int maxRadius = NodeDrawConfig.getMaxNodeRadius();

    g.draw3DRect( originX - maxRadius, originY - maxRadius, maxRadius << 1, maxRadius << 1, false );

    NodeTask nodeTask = (NodeTask)node.getNode();

    String taskName = nodeTask.getTaskName();

    int width = g.getFontMetrics().stringWidth( taskName );
    int height = g.getFontMetrics().getHeight();
    g.drawString( taskName, originX - (width >> 1), originY - (height >> 1) );
  }

  @Override
  public int getOffset ()
  {
    return NodeDrawConfig.getMaxNodeRadius();
  }
}
