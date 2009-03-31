package com.googlecode.sarasvati.editor.command;

import java.awt.Point;

import org.netbeans.api.visual.widget.Widget;

public class MoveNodeCommand implements Command
{
  private final Point startLocation;
  private final Point endLocation;

  private final Widget widget;

  public MoveNodeCommand (Widget widget, Point startLocation, Point endLocation)
  {
    this.widget = widget;
    this.startLocation = startLocation;
    this.endLocation = endLocation;
  }

  @Override
  public void performAction ()
  {
    widget.setPreferredLocation( endLocation );
    widget.revalidate();
    widget.getScene().validate();
  }

  @Override
  public void undoAction ()
  {
    widget.setPreferredLocation( startLocation );
    widget.revalidate();
    widget.getScene().validate();
  }
}