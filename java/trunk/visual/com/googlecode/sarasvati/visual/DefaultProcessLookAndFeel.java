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
import javax.swing.JLabel;

import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.icon.TaskIcon;
import com.googlecode.sarasvati.visual.process.SarasvatiProcessScene;
import com.googlecode.sarasvati.visual.process.VisualProcessNode;

/**
 * Generates widgets using a TaskIcon for nodes of type 'task', and DefaultNodeIcon
 * for all other node types.
 *
 * @author Paul Lorenz
 */
public class DefaultProcessLookAndFeel implements ProcessLookAndFeel
{
  public static final DefaultProcessLookAndFeel INSTANCE = new DefaultProcessLookAndFeel( false, true );

  protected boolean drawSelfArcs;
  protected boolean drawArcLabels;

  public DefaultProcessLookAndFeel (boolean drawSelfArcs, boolean drawArcLabels)
  {
    this.drawSelfArcs = drawSelfArcs;
    this.drawArcLabels = drawArcLabels;
  }

  @Override
  public boolean drawArcLabels (Arc arc)
  {
    return drawArcLabels;
  }

  @Override
  public boolean drawSelfArcs (Arc arc)
  {
    return drawSelfArcs;
  }

  @Override
  public Widget newWidget (VisualProcessNode node, SarasvatiProcessScene scene)
  {
    Icon icon = null;

    if ( getTaskType().equals( node.getNode().getType() ) )
    {
      icon = new TaskIcon( node.getNode(), node.getToken() );
    }
    else
    {
      icon = new DefaultNodeIcon( node.getNode(), node.getToken() );
    }

    JLabel label = new JLabel( icon );
    label.setSize( icon.getIconWidth(), icon.getIconHeight() );
    return new ComponentWidget( scene, label );
  }

  protected String getTaskType ()
  {
    return "task";
  }
  
}
