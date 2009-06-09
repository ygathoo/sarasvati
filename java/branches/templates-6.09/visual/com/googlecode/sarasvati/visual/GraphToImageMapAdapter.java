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
import com.googlecode.sarasvati.visual.icon.TaskIcon;

/**
 * Provides default implementations for methods in the
 * {@link GraphToImageMap} interface. All methods return
 * null unless overridden except the iconForNode method,
 * which returns either a {@link TaskIcon} or {@link DefaultNodeIcon}
 * depending on the type of the node passed in.
 *
 * @author Paul Lorenz
 * @author chungonn
 */
public class GraphToImageMapAdapter implements GraphToImageMap
{
  
  private final String taskType;

  public GraphToImageMapAdapter ()
  {
    this("task");
  }
  
  public GraphToImageMapAdapter (String taskType){
    this.taskType = taskType;
  }
  
  /**
   * Returns true unless overridden
   * @see com.googlecode.sarasvati.visual.GraphToImageMap#drawArcLabels()
   */
  @Override
  public boolean drawArcLabels ()
  {
    return true;
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

  /**
   * Returns a {@link TaskIcon} for nodes with type of 'task' and a
   * {@link DefaultNodeIcon} for all other nodes.
   *
   * @see com.googlecode.sarasvati.visual.GraphToImageMap#iconForNode(com.googlecode.sarasvati.Node)
   */
  @Override
  public Icon iconForNode (Node node)
  {
    if ( taskType.equalsIgnoreCase( node.getType() ) )
    {
      return new TaskIcon( node, null );
    }
    return new DefaultNodeIcon( node, null );
  }

  /**
   * Returns null unless overridden.
   */
  @Override
  public String hoverForArc (Arc arc)
  {
    return null;
  }

  /**
   * Returns null unless overridden.
   */
  @Override
  public String hoverForNode (Node node)
  {
    return null;
  }

  /**
   * Returns null unless overridden.
   */
  @Override
  public String hrefForArc (Arc arc)
  {
    return null;
  }

  /**
   * Returns null unless overridden.
   */
  @Override
  public String hrefForNode (Node node)
  {
    return null;
  }
}
