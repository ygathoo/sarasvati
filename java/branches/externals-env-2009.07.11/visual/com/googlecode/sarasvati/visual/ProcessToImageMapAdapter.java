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

import javax.swing.Icon;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.icon.EndNodeIcon;
import com.googlecode.sarasvati.visual.icon.TaskIcon;
import com.googlecode.sarasvati.visual.process.VisualProcessArc;
import com.googlecode.sarasvati.visual.process.VisualProcessNode;

/**
 * Provides default implementations for methods in the
 * {@link ProcessToImageMap} interface. All methods return
 * null unless overridden.
 *
 * @author Paul Lorenz
 * @author chungonn
 */
public class ProcessToImageMapAdapter implements ProcessToImageMap
{
  private final String taskType;

  public ProcessToImageMapAdapter ()
  {
    this("task");
  }

  public ProcessToImageMapAdapter (String taskType)
  {
    this.taskType = taskType;
  }

  /**
   * Returns true unless overridden
   * @see com.googlecode.sarasvati.visual.ProcessToImageMap#drawArcLabels(Arc)
   */
  @Override
  public boolean drawArcLabels (Arc arc)
  {
    return true;
  }

  /**
   * Returns the defaultValue for all arcs
   *
   * @see com.googlecode.sarasvati.visual.ProcessToImageMap#isBackArc(com.googlecode.sarasvati.Arc)
   */
  @Override
  public boolean isBackArc (Arc arc, boolean defaultValue)
  {
    return defaultValue;
  }

  /**
   * Returns a {@link TaskIcon} for nodes with type of 'task' and a
   * {@link DefaultNodeIcon} for all other nodes.
   *
   * @see com.googlecode.sarasvati.visual.GraphToImageMap#iconForNode(com.googlecode.sarasvati.Node)
   */
  @Override
  public Icon iconForNode (VisualProcessNode node)
  {
    if ( getTaskType().equalsIgnoreCase( node.getNode().getType() ) )
    {
      return new TaskIcon( node.getNode(), node.getToken() );
    }
    else if ( node.getNode().getType().equalsIgnoreCase( "end" ) )
    {
      return new EndNodeIcon();
    }
    return new DefaultNodeIcon( node.getNode(), node.getToken() );
  }

  /**
   * TaskType default is "task". It is used to determine the type of node icon
   * returns by {@link #iconForNode(Node)}
   *
   */
  public String getTaskType ()
  {
    return taskType;
  }

  @Override
  public String hoverForArc (VisualProcessArc arc)
  {
    return null;
  }

  @Override
  public String hoverForNode (VisualProcessNode node)
  {
    return null;
  }

  @Override
  public String hrefForArc (VisualProcessArc arc)
  {
    return null;
  }

  @Override
  public String hrefForNode (VisualProcessNode node)
  {
    return null;
  }
}
