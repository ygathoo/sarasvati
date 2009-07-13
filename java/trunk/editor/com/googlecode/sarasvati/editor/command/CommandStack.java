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

    Copyright 2009 Paul Lorenz
 */
package com.googlecode.sarasvati.editor.command;

import java.awt.Point;
import java.util.LinkedList;

import com.googlecode.sarasvati.editor.GraphEditor;
import com.googlecode.sarasvati.editor.model.ArcState;
import com.googlecode.sarasvati.editor.model.EditorArc;
import com.googlecode.sarasvati.editor.model.EditorExternal;
import com.googlecode.sarasvati.editor.model.EditorGraphMember;
import com.googlecode.sarasvati.editor.model.EditorNode;
import com.googlecode.sarasvati.editor.model.EditorScene;
import com.googlecode.sarasvati.editor.model.GraphMemberState;

public class CommandStack
{
  private static CommandStack current = new CommandStack ();

  private final LinkedList<Command> commandStack = new LinkedList<Command>();
  private Command lastSavedCommand = null;

  private int currentIndex = -1;

  public void pushCommand (final Command command)
  {
    currentIndex++;
    while ( currentIndex < commandStack.size() )
    {
      commandStack.removeLast();
    }
    commandStack.add( command );
    GraphEditor.getInstance().updateUndoRedoSave();
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
    GraphEditor.getInstance().updateUndoRedoSave();
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
    GraphEditor.getInstance().updateUndoRedoSave();
  }

  public String getUndoName ()
  {
    return canUndo() ? commandStack.get( currentIndex ).getName() : "";
  }

  public String getRedoName ()
  {
    return canRedo() ? commandStack.get( currentIndex + 1 ).getName() : "";
  }

  public void saved ()
  {
    if ( currentIndex >= 0 )
    {
      lastSavedCommand = commandStack.get( currentIndex );
    }
    GraphEditor.getInstance().updateUndoRedoSave();
  }

  public boolean isSaved ()
  {
    return commandStack.isEmpty() ||
           ( currentIndex >= 0 && commandStack.get( currentIndex ) == lastSavedCommand);
  }

  public static void markSaved ()
  {
    current.saved();
  }

  public static void nodeMoved (final EditorScene scene,
                                final EditorGraphMember<?> member,
                                final Point startLocation,
                                final Point endLocation)
  {
    member.setOrigin( new Point( endLocation ) );
    current.pushCommand( new MoveNodeCommand( scene, member, startLocation, endLocation ) );
  }

  public static void moveNode (final EditorScene scene,
                               final EditorGraphMember<?> member,
                               final Point startLocation,
                               final Point endLocation)
  {
    pushAndPerform( new MoveNodeCommand( scene, member, startLocation, endLocation ) );
  }

  public static void addNode (final EditorScene scene,
                              final Point location,
                              final EditorNode node)
  {
    pushAndPerform( new AddNodeCommand( scene, location, node ) );
  }

  public static void addExternal (final EditorScene scene,
                                  final Point location,
                                  final EditorExternal external)
  {
    pushAndPerform( new AddExternalCommand( scene, location, external ) );
  }

  public static void addArc (final EditorScene scene,
                             final EditorArc arc)
  {
    pushAndPerform( new NewArcCommand( scene, arc ) );
  }

  public static void deleteArc (final EditorScene scene,
                                final EditorArc arc)
  {
    pushAndPerform( new DeleteArcCommand( scene, arc ) );
  }

  public static <T extends GraphMemberState> void editNode (final EditorScene scene,
                                                            final EditorGraphMember<T> graphMember,
                                                            final T newState)
  {
    pushAndPerform( new EditNodeCommand<T>( scene, graphMember, newState ) );
  }

  public static void editArc (final EditorScene scene,
                              final EditorArc arc,
                              final ArcState newState)
  {
    pushAndPerform( new EditArcCommand( scene, arc, newState ) );
  }

  public static void pushAndPerform (final Command command)
  {
    current.pushCommand( command );
    command.performAction();
  }

  public static void updateArc (final EditorScene scene,
                                final EditorArc arc,
                                final boolean isSource,
                                final EditorGraphMember<?> newNode)
  {
    ChangeArcCommand command = new ChangeArcCommand( scene, arc, isSource, newNode );
    current.pushCommand( command );
    command.performAction();
  }

  public static CommandStack getCurrent ()
  {
    return current;
  }

  public static void setCurrent (final CommandStack stack)
  {
    current = stack;
  }
}