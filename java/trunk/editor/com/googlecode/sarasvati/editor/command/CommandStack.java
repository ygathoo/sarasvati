package com.googlecode.sarasvati.editor.command;

import java.awt.Point;
import java.util.LinkedList;

import org.netbeans.api.visual.widget.Widget;

public class CommandStack
{
  private static CommandStack current = new CommandStack ();

  private final LinkedList<Command> commandStack = new LinkedList<Command>();
  private int currentIndex = -1;

  public void pushCommand (Command command)
  {
    currentIndex++;
    while ( currentIndex < commandStack.size() )
    {
      commandStack.removeLast();
    }
    commandStack.add( command );
  }

  public boolean canUndo ()
  {
    return currentIndex > -1;
  }

  public boolean canRedo ()
  {
    return currentIndex < ( commandStack.size() - 1 );
  }

  public void undo ()
  {
    if ( !canUndo() )
    {
      return;
    }

    Command command = commandStack.get( currentIndex );
    currentIndex--;
    command.undoAction();
  }

  public void redo ()
  {
    if ( !canRedo() )
    {
      return;
    }

    currentIndex++;
    Command command = commandStack.get( currentIndex );
    command.performAction();
  }

  public static void nodeMoved (Widget widget, Point startLocation, Point endLocation)
  {
    current.pushCommand( new MoveNodeCommand( widget, startLocation, endLocation ) );
  }

  public static CommandStack getCurrent ()
  {
    return current;
  }
}