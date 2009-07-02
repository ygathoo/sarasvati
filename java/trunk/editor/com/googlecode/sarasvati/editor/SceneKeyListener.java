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

public class SceneKeyListener extends WidgetAction.Adapter
{
  private EditorMode previousMode = null;

  @Override
  public State keyPressed (Widget widget, WidgetKeyEvent event)
  {
    System.out.println( "widget: " + widget + " event: " + event );
    EditorMode mode = null;

    if ( event.getKeyChar() == 'a' )
    {
      mode = EditorMode.AddNode;
    }
    else if ( event.getKeyChar() == 'e' )
    {
      mode = EditorMode.EditArcs;
    }

    if ( mode != null )
    {
      GraphEditor editor = GraphEditor.getInstance();

      if ( previousMode != null )
      {
        previousMode = editor.getMode();
      }

      editor.setMode( mode );
      return State.CONSUMED;
    }

    return State.REJECTED;
  }

  @Override
  public State keyReleased (Widget widget, WidgetKeyEvent event)
  {
    if ( previousMode != null )
    {
      GraphEditor.getInstance().setMode( previousMode );
      return State.CONSUMED;
    }
    return State.REJECTED;
  }
}
