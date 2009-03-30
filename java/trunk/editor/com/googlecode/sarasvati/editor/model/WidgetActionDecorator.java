package com.googlecode.sarasvati.editor.model;

import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.widget.Widget;

public class WidgetActionDecorator implements WidgetAction
{
  protected WidgetAction action;

  public WidgetActionDecorator (WidgetAction action)
  {
    this.action = action;
  }

  public WidgetAction getAction ()
  {
    return action;
  }

  public State dragEnter( Widget arg0, WidgetDropTargetDragEvent arg1 )
  {
    return action.dragEnter( arg0, arg1 );
  }

  public State dragExit( Widget arg0, WidgetDropTargetEvent arg1 )
  {
    return action.dragExit( arg0, arg1 );
  }

  public State dragOver( Widget arg0, WidgetDropTargetDragEvent arg1 )
  {
    return action.dragOver( arg0, arg1 );
  }

  public State drop( Widget arg0, WidgetDropTargetDropEvent arg1 )
  {
    return action.drop( arg0, arg1 );
  }

  public State dropActionChanged( Widget arg0, WidgetDropTargetDragEvent arg1 )
  {
    return action.dropActionChanged( arg0, arg1 );
  }

  public State focusGained( Widget arg0, WidgetFocusEvent arg1 )
  {
    return action.focusGained( arg0, arg1 );
  }

  public State focusLost( Widget arg0, WidgetFocusEvent arg1 )
  {
    return action.focusLost( arg0, arg1 );
  }

  public State keyPressed( Widget arg0, WidgetKeyEvent arg1 )
  {
    return action.keyPressed( arg0, arg1 );
  }

  public State keyReleased( Widget arg0, WidgetKeyEvent arg1 )
  {
    return action.keyReleased( arg0, arg1 );
  }

  public State keyTyped( Widget arg0, WidgetKeyEvent arg1 )
  {
    return action.keyTyped( arg0, arg1 );
  }

  public State mouseClicked( Widget arg0, WidgetMouseEvent arg1 )
  {
    return action.mouseClicked( arg0, arg1 );
  }

  public State mouseDragged( Widget arg0, WidgetMouseEvent arg1 )
  {
    return action.mouseDragged( arg0, arg1 );
  }

  public State mouseEntered( Widget arg0, WidgetMouseEvent arg1 )
  {
    return action.mouseEntered( arg0, arg1 );
  }

  public State mouseExited( Widget arg0, WidgetMouseEvent arg1 )
  {
    return action.mouseExited( arg0, arg1 );
  }

  public State mouseMoved( Widget arg0, WidgetMouseEvent arg1 )
  {
    return action.mouseMoved( arg0, arg1 );
  }

  public State mousePressed( Widget arg0, WidgetMouseEvent arg1 )
  {
    return action.mousePressed( arg0, arg1 );
  }

  public State mouseReleased( Widget arg0, WidgetMouseEvent arg1 )
  {
    return action.mouseReleased( arg0, arg1 );
  }

  public State mouseWheelMoved( Widget arg0, WidgetMouseWheelEvent arg1 )
  {
    return action.mouseWheelMoved( arg0, arg1 );
  }
}
