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

import javax.swing.JLabel;

import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.icon.TaskIcon;
import com.googlecode.sarasvati.visual.process.ProcessTreeNode;
import com.googlecode.sarasvati.visual.process.SarasvatiProcessScene;

/**
 * Generates widgets using a TaskIcon for nodes of type 'task', and DefaultNodeIcon
 * for all other node types.
 *
 * @author Paul Lorenz
 */
public class DefaultProcessTreeNodeWidgetFactory implements ProcessTreeNodeWidgetFactory
{
  public static final DefaultProcessTreeNodeWidgetFactory INSTANCE = new DefaultProcessTreeNodeWidgetFactory();

  @Override
  public Widget newWidget (ProcessTreeNode node, SarasvatiProcessScene scene)
  {
    JLabel label = null;

    if ( "task".equals( node.getNode().getType() ) )
    {
      label =  new JLabel( new TaskIcon( node.getNode(), node.getToken() ) );
    }
    else
    {
      label = new JLabel( new DefaultNodeIcon( node.getNode(), node.getToken() ) );
    }

    return new ComponentWidget( scene, label );
  }
}
