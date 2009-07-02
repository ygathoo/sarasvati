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

    Copyright 2009 Paul Lorenz
 */
package com.googlecode.sarasvati.editor;

import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.widget.Widget;

public abstract class ToggledWidgetActionDecorator implements WidgetAction
{
  protected WidgetAction action;

  public abstract boolean isEnabled ();

  public ToggledWidgetActionDecorator (WidgetAction action)
  {
    this.action = action;
  }

  public WidgetAction getAction ()
  {
    return action;
  }

  public State dragEnter( Widget arg0, WidgetDropTargetDragEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.dragEnter( arg0, arg1 );
  }

  public State dragExit( Widget arg0, WidgetDropTargetEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.dragExit( arg0, arg1 );
  }

  public State dragOver( Widget arg0, WidgetDropTargetDragEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.dragOver( arg0, arg1 );
  }

  public State drop( Widget arg0, WidgetDropTargetDropEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.drop( arg0, arg1 );
  }

  public State dropActionChanged( Widget arg0, WidgetDropTargetDragEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.dropActionChanged( arg0, arg1 );
  }

  public State focusGained( Widget arg0, WidgetFocusEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.focusGained( arg0, arg1 );
  }

  public State focusLost( Widget arg0, WidgetFocusEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.focusLost( arg0, arg1 );
  }

  public State keyPressed( Widget arg0, WidgetKeyEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.keyPressed( arg0, arg1 );
  }

  public State keyReleased( Widget arg0, WidgetKeyEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.keyReleased( arg0, arg1 );
  }

  public State keyTyped( Widget arg0, WidgetKeyEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.keyTyped( arg0, arg1 );
  }

  public State mouseClicked( Widget arg0, WidgetMouseEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mouseClicked( arg0, arg1 );
  }

  public State mouseDragged( Widget arg0, WidgetMouseEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mouseDragged( arg0, arg1 );
  }

  public State mouseEntered( Widget arg0, WidgetMouseEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mouseEntered( arg0, arg1 );
  }

  public State mouseExited( Widget arg0, WidgetMouseEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mouseExited( arg0, arg1 );
  }

  public State mouseMoved( Widget arg0, WidgetMouseEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mouseMoved( arg0, arg1 );
  }

  public State mousePressed( Widget arg0, WidgetMouseEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mousePressed( arg0, arg1 );
  }

  public State mouseReleased( Widget arg0, WidgetMouseEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mouseReleased( arg0, arg1 );
  }

  public State mouseWheelMoved( Widget arg0, WidgetMouseWheelEvent arg1 )
  {
    if ( !isEnabled() )
    {
      return State.REJECTED;
    }

    return action.mouseWheelMoved( arg0, arg1 );
  }
}