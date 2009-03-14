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
