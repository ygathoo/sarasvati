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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.draw2d.geometry.PointList;
import org.eclipse.draw2d.graph.Path;
import org.netbeans.api.visual.anchor.Anchor;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.Scene;

import com.googlecode.sarasvati.visual.util.ConvertUtil;

public class PathTrackingConnectionWidget extends ConnectionWidget
{
  protected ShortestPathRouterAdapter router;

  protected Point start = null;
  protected Point end   = null;

  protected Path path;
  protected boolean resetControlPoints = false;

  protected List<Point> route = null;

  public PathTrackingConnectionWidget (ShortestPathRouterAdapter router, Scene scene)
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
      Point newStart = sourceAnchor.compute( getSourceAnchorEntry() ).getAnchorSceneLocation();
      Point newEnd = targetAnchor.compute( getTargetAnchorEntry() ).getAnchorSceneLocation();
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
        router.addPath( path );
        resetControlPoints = true;
        route = null;
      }
    }
    else
    {
      router.removePath( path );
      start = null;
      end   = null;
      path  = null;
      route = null;
    }
  }

  public List<Point> getRoute ()
  {
    if ( path == null )
    {
      return Collections.emptyList();
    }

    if ( route == null )
    {
      PointList pointList = path.getPoints();

      route = new ArrayList<Point>( pointList.size() );

      for ( int i = 0; i < pointList.size(); i++ )
      {
        Point point = ConvertUtil.swtToAwt( pointList.getPoint( i ) );
        route.add( point );
      }
    }

    return route;
  }
}