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

import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.icon.TaskIcon;
import com.googlecode.sarasvati.visual.process.VisualProcessArc;
import com.googlecode.sarasvati.visual.process.VisualProcessNode;

/**
 * Provides default implementations for methods in the
 * {@link ProcessToImageMap} interface. All methods return
 * null unless overridden.
 *
 * @author Paul Lorenz
 */
public class ProcessToImageMapAdapter implements ProcessToImageMap
{
  /**
   * Returns true unless overridden
   * @see com.googlecode.sarasvati.visual.ProcessToImageMap#drawArcLabels()
   */
  @Override
  public boolean drawArcLabels ()
  {
    return true;
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
    if ( "task".equalsIgnoreCase( node.getNode().getType() ) )
    {
      return new TaskIcon( node.getNode(), node.getToken() );
    }
    return new DefaultNodeIcon( node.getNode(), node.getToken() );
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
