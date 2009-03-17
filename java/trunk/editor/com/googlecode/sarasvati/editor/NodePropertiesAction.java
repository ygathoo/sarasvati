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
package com.googlecode.sarasvati.editor;

import java.awt.event.MouseEvent;

import javax.swing.JDialog;

import org.netbeans.api.visual.action.WidgetAction.Adapter;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.editor.model.EditorNode;
import com.googlecode.sarasvati.editor.model.EditorScene;

public class NodePropertiesAction extends Adapter
{
  @Override
  public State mouseClicked (Widget widget, WidgetMouseEvent event)
  {
    if ( event.getClickCount() == 1 && event.getButton() == MouseEvent.BUTTON3 )
    {
      EditorScene scene = (EditorScene)widget.getScene();
      EditorNode node = (EditorNode) scene.findObject( widget );

      JDialog dialog = DialogFactory.newPropertiesDialog( node );
      dialog.setLocation( widget.convertLocalToScene( event.getPoint() ) );
      dialog.setVisible( true );

      return State.CONSUMED;
    }

    return State.REJECTED;
  }

}
