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
package com.googlecode.sarasvati.visual;

import java.awt.Point;
import java.awt.Rectangle;

public class ConvertUtil
{
  public static org.eclipse.draw2d.geometry.Rectangle awtToSwt (Rectangle r)
  {
    return r == null ? null : new org.eclipse.draw2d.geometry.Rectangle( r.x, r.y, r.width, r.height );
  }

  public static org.eclipse.draw2d.geometry.Point awtToSwt (Point point)
  {
    return new org.eclipse.draw2d.geometry.Point( point.x, point.y );
  }

  public static Rectangle swtToAwt (org.eclipse.draw2d.geometry.Rectangle r)
  {
    return new Rectangle( r.x, r.y, r.width, r.height );
  }

  public static Point swtToAwt (org.eclipse.draw2d.geometry.Point point)
  {
    return new Point( point.x, point.y );
  }
}
