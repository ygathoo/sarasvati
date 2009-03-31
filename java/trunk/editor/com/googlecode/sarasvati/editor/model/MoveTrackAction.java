/**
 *
 */
package com.googlecode.sarasvati.editor.model;

import java.awt.Point;
import java.awt.event.MouseEvent;

import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.editor.command.CommandStack;

public class MoveTrackAction extends WidgetActionDecorator
{
  protected Widget currentWidget = null;
  protected Point startLocation = null;

  public MoveTrackAction (WidgetAction action)
  {
    super( action );
  }

  @Override
  public State mousePressed (Widget widget, WidgetMouseEvent event )
  {
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
    State state = getAction().mouseReleased( widget, event );
    if ( currentWidget == widget )
    {
      CommandStack.nodeMoved( widget, startLocation, widget.getLocation() );
    }
    return state;
  }

}