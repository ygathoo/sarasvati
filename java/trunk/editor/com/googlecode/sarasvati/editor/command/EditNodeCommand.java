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

import com.googlecode.sarasvati.editor.model.EditorGraphMember;
import com.googlecode.sarasvati.editor.model.EditorScene;
import com.googlecode.sarasvati.editor.model.GraphMemberState;

public class EditNodeCommand<T extends GraphMemberState> implements Command
{
  private final EditorScene scene;
  private final EditorGraphMember<T> graphMember;
  private final T                    newState;

  public EditNodeCommand (final EditorScene scene,
                          final EditorGraphMember<T> graphMember,
                          final T newState)
  {
    this.scene = scene;
    this.graphMember = graphMember;
    this.newState = newState;
  }

  @Override
  public void performAction ()
  {
    graphMember.pushState( newState );
    scene.validate();
  }

  @Override
  public void undoAction ()
  {
    graphMember.popState();
    scene.validate();
  }

  @Override
  public String getName ()
  {
    return "Edit Node";
  }
}