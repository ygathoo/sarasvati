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

    Copyright 2008-2009 Paul Lorenz
*/
package com.googlecode.sarasvati.editor;

import java.awt.Point;
import java.awt.event.MouseEvent;

import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.JoinType;
import com.googlecode.sarasvati.editor.command.CommandStack;
import com.googlecode.sarasvati.editor.model.EditorNode;
import com.googlecode.sarasvati.editor.model.EditorScene;
import com.googlecode.sarasvati.editor.model.NodeState;

public class SceneAddNodeAction extends WidgetAction.Adapter
{
  public static SceneAddNodeAction INSTANCE = new SceneAddNodeAction();

  protected static boolean enabled = false;

  public static void setEnabled (boolean enabled)
  {
    SceneAddNodeAction.enabled = enabled;
  }

  @Override
  public State mouseClicked (Widget widget, WidgetMouseEvent event)
  {
    if( !enabled )
    {
      return State.REJECTED;
    }

    if ( event.getClickCount() == 1 && event.getButton() == MouseEvent.BUTTON1 )
    {
      NodeState state = new NodeState( "New Node", "node", JoinType.OR, null, false, null, null );
      EditorNode node = new EditorNode( state );

      EditorScene scene = (EditorScene)widget.getScene();
      Point location = widget.convertLocalToScene( event.getPoint() );

      CommandStack.addNode( scene, location, node );

      return State.CONSUMED;
    }

    return State.REJECTED;
  }
}
