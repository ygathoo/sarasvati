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

package com.googlecode.sarasvati.util;

import org.eclipse.draw2d.geometry.PointList;
import org.eclipse.draw2d.graph.Path;

public class SvGraphicsUtil
{
  public static void addOffsetBendToPath (final Path path, final int spacing)
  {
    PointList bendPoints = new PointList();
    int deltaX = path.getEndPoint().x - path.getStartPoint().x;
    int y = ((path.getEndPoint().y + path.getStartPoint().y) >> 1 ) + Math.min( Math.abs( deltaX ), spacing );

    int deltaY = path.getEndPoint().y - path.getStartPoint().y;
    int offset = Math.min( Math.abs( deltaY ), spacing );
    if ( ( deltaX > 0 && deltaY > 0 ) || (deltaX < 0 && deltaY < 0 ) )
    {
      offset = -offset;
    }

    int x = ((path.getEndPoint().x + path.getStartPoint().x) >> 1 ) + offset;

    bendPoints.addPoint( x, y );
    //Add a new obstacle
    path.setBendPoints( bendPoints  );
  }
}
