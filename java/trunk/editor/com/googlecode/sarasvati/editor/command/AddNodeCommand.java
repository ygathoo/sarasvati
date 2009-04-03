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

import com.googlecode.sarasvati.editor.model.EditorNode;
import com.googlecode.sarasvati.editor.model.EditorScene;

public class AddNodeCommand implements Command
{
  private EditorScene scene;
  private Point location;
  private EditorNode node;

  public AddNodeCommand (EditorScene scene, Point location, EditorNode node)
  {
    this.scene = scene;
    this.location = location;
    this.node = node;
  }

  @Override
  public void performAction ()
  {
    node.setOrigin( location );
    scene.addNode( node );
    scene.validate();
  }

  @Override
  public void undoAction ()
  {
    scene.removeNode( node );
    scene.validate();
  }

  @Override
  public String getName ()
  {
    return "Add Node";
  }
}