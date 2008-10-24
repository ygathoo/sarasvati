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

import java.awt.Polygon;

import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;

public class ConvertUtil
{
  public static Rectangle awtToSwt (java.awt.Rectangle r)
  {
    return r == null ? null : new Rectangle( r.x, r.y, r.width, r.height );
  }

  public static Point awtToSwt (java.awt.Point point)
  {
    return new Point( point.x, point.y );
  }

  public static java.awt.Rectangle swtToAwt (Rectangle r)
  {
    return new java.awt.Rectangle( r.x, r.y, r.width, r.height );
  }

  public static java.awt.Point swtToAwt (Point point)
  {
    return new java.awt.Point( point.x, point.y );
  }

  public static void appendPolygon (Polygon poly, StringBuilder buf)
  {
    if ( poly.npoints > 0 )
    {
      buf.append( poly.xpoints[0] );
      buf.append( "," );
      buf.append( poly.ypoints[1] );
    }

    for ( int i = 0; i < poly.npoints; i++ )
    {
      buf.append( ", " );
      buf.append( poly.xpoints[i] );
      buf.append( "," );
      buf.append( poly.ypoints[i] );
    }
  }
}
