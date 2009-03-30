package com.googlecode.sarasvati.editor.command;

import java.util.LinkedList;

public class CommandStack
{
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
    return currentIndex < ( commandStack.size() - 2 );
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
}