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
package com.googlecode.sarasvati.editor.model;

import java.awt.Point;

public class EditorGraphMember
{
  protected Point  origin = new Point();

  protected String name;

  public int getX ()
  {
    return origin.x;
  }

  public void setX (int x)
  {
    this.origin.x = x;
  }

  public int getY ()
  {
    return origin.y;
  }

  public void setY (int y)
  {
    this.origin.y = y;
  }

  public Point getOrigin ()
  {
    return origin;
  }

  public void setOrigin (Point origin)
  {
    this.origin = origin;
  }

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }
}