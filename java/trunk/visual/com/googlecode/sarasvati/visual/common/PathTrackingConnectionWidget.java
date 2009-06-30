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
package com.googlecode.sarasvati.visual.common;

import java.awt.Point;
import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.draw2d.geometry.PointList;
import org.eclipse.draw2d.graph.Path;
import org.netbeans.api.visual.anchor.Anchor;
import org.netbeans.api.visual.widget.ConnectionWidget;

import com.googlecode.sarasvati.visual.util.ConvertUtil;

public class PathTrackingConnectionWidget extends ConnectionWidget
{
  protected ShortestPathRouterAdapter router;

  protected Point start = null;
  protected Point end   = null;

  protected Path oldPath = null;
  protected Path path;
  protected boolean resetControlPoints = false;

  public PathTrackingConnectionWidget (ShortestPathRouterAdapter router, GraphSceneImpl<?,?> scene)
  {
    super( scene );
    this.router = router;
  }

  public void ensurePathCurrent ()
  {
    Anchor sourceAnchor = getSourceAnchor();
    Anchor targetAnchor = getTargetAnchor();

    if ( sourceAnchor != null && targetAnchor != null )
    {
      Point newStart = null;
      Point newEnd = null;

      boolean isSelfArc = sourceAnchor.getRelatedWidget().equals( targetAnchor.getRelatedWidget() );
      if ( isSelfArc )
      {
        Point origin = sourceAnchor.getRelatedWidget().getLocation();
        Rectangle bounds = sourceAnchor.getRelatedWidget().getBounds();
        newStart = new Point( origin.x + (bounds.width >> 1 ), origin.y + bounds.height );
        newEnd = new Point( origin.x + bounds.width, origin.y + (bounds.height >> 1 ) );
      }
      else
      {
        newStart = sourceAnchor.compute( getSourceAnchorEntry() ).getAnchorSceneLocation();
        newEnd = targetAnchor.compute( getTargetAnchorEntry() ).getAnchorSceneLocation();
      }

      boolean pathChange = path == null || !start.equals( newStart ) || !end.equals( newEnd );
      if ( path != null && pathChange )
      {
        router.removePath( path );
      }
      if ( pathChange )
      {
        start = newStart;
        end   = newEnd;
        path = new Path( ConvertUtil.awtToSwt( start ), ConvertUtil.awtToSwt( end ) );
        router.addPath( path, isSelfArc );
        resetControlPoints = true;
      }
    }
    else
    {
      router.removePath( path );
      start = null;
      end   = null;
      path  = null;
    }
  }

  public List<Point> getRoute ()
  {
    if ( path == null )
    {
      return Collections.emptyList();
    }

    PointList pointList = path.getPoints();

    // Route is apparently not cache-able, b/c when I try,
    // connections no longer route properly.
    ArrayList<Point> route = new ArrayList<Point>( pointList.size() );

    for ( int i = 0; i < pointList.size(); i++ )
    {
      Point point = ConvertUtil.swtToAwt( pointList.getPoint( i ) );
      route.add( point );
    }

    return route;
  }
}