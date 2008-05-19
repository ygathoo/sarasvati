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
