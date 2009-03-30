package com.googlecode.sarasvati.editor.command;

import java.awt.Point;

import org.netbeans.api.visual.widget.Widget;

public class MoveNodeCommand implements Command
{
  private final Point startLocation;
  private final Point endLocation;

  private final Widget widget;

  public MoveNodeCommand (Point startLocation, Point endLocation, Widget widget)
  {
    this.startLocation = startLocation;
    this.endLocation = endLocation;
    this.widget = widget;
  }

  @Override
  public void performAction ()
  {
    widget.setPreferredLocation( endLocation );
  }

  @Override
  public void undoAction ()
  {
    widget.setPreferredLocation( startLocation );
  }
}