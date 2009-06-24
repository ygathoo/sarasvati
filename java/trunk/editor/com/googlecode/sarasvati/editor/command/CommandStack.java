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
package com.googlecode.sarasvati.editor.command;

import java.awt.Point;
import java.util.LinkedList;

import com.googlecode.sarasvati.editor.GraphEditor;
import com.googlecode.sarasvati.editor.model.EditorArc;
import com.googlecode.sarasvati.editor.model.EditorGraphMember;
import com.googlecode.sarasvati.editor.model.EditorNode;
import com.googlecode.sarasvati.editor.model.EditorScene;

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
    GraphEditor.getInstance().updateUndoRedo();
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
    GraphEditor.getInstance().updateUndoRedo();
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
    GraphEditor.getInstance().updateUndoRedo();
  }

  public String getUndoName ()
  {
    return canUndo() ? commandStack.get( currentIndex ).getName() : "";
  }

  public String getRedoName ()
  {
    return canRedo() ? commandStack.get( currentIndex + 1 ).getName() : "";
  }

  public static void nodeMoved (EditorScene scene, EditorGraphMember member, Point startLocation, Point endLocation)
  {
    current.pushCommand( new MoveNodeCommand( scene, member, startLocation, endLocation ) );
  }

  public static void addNode (EditorScene scene, Point location, EditorNode node)
  {
    pushAndPerform( new AddNodeCommand( scene, location, node ) );
  }

  public static void addArc (EditorScene scene, EditorArc arc)
  {
    pushAndPerform( new NewArcCommand( scene, arc ) );
  }

  public static void deleteArc (EditorScene scene, EditorArc arc)
  {
    pushAndPerform( new DeleteArcCommand( scene, arc ) );
  }

  private static void pushAndPerform (Command command)
  {
    current.pushCommand( command );
    command.performAction();
  }

  public static void updateArc (EditorScene scene, EditorArc arc, boolean isSource, EditorGraphMember newNode)
  {
    ChangeArcCommand command = new ChangeArcCommand( scene, arc, isSource, newNode );
    current.pushCommand( command );
    command.performAction();
  }

  public static CommandStack getCurrent ()
  {
    return current;
  }

  public static void setCurrent (CommandStack stack)
  {
    current = stack;
  }
}