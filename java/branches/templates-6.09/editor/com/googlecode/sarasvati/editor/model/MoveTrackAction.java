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
package com.googlecode.sarasvati.editor.model;

import java.awt.Point;
import java.awt.event.MouseEvent;

import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.editor.command.CommandStack;

public class MoveTrackAction extends WidgetActionDecorator
{
  protected static boolean enabled = false;

  public static void setEnabled (boolean enabled)
  {
    MoveTrackAction.enabled = enabled;
  }

  protected Widget currentWidget = null;
  protected Point startLocation = null;

  public MoveTrackAction (WidgetAction action)
  {
    super( action );
  }

  @Override
  public State mousePressed (Widget widget, WidgetMouseEvent event)
  {
    if ( !enabled )
    {
      return State.REJECTED;
    }

    if ( event.getButton() == MouseEvent.BUTTON1 && event.getClickCount() == 1 )
    {
      currentWidget = widget;
      startLocation = widget.getPreferredLocation();
      super.mousePressed( widget, event );
      return State.createLocked( widget, this );
    }

    return getAction().mousePressed( widget, event );
  }

  @Override
  public State mouseDragged (Widget widget, WidgetMouseEvent event)
  {
    if ( !enabled )
    {
      return State.REJECTED;
    }

    if ( currentWidget == widget )
    {
      super.mouseDragged( widget, event );
      return State.createLocked( widget, this );
    }

    return super.mouseDragged( widget, event );
  }

  @Override
  public State mouseReleased (Widget widget, WidgetMouseEvent event)
  {
    if ( !enabled )
    {
      return State.REJECTED;
    }

    State state = getAction().mouseReleased( widget, event );
    if ( currentWidget == widget )
    {
      EditorScene scene = (EditorScene)widget.getScene();
      EditorGraphMember member = (EditorGraphMember) scene.findObject( widget );
      CommandStack.nodeMoved( scene, member, startLocation, widget.getLocation() );
    }
    return state;
  }
}