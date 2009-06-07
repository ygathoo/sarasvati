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
import java.util.List;

import org.eclipse.draw2d.geometry.PointList;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.draw2d.graph.Path;
import org.eclipse.draw2d.graph.ShortestPathRouter;
import org.netbeans.api.visual.router.Router;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.visual.util.ConvertUtil;

public class ShortestPathRouterAdapter implements Router
{
  protected GraphSceneImpl<?, ?> scene;
  protected ShortestPathRouter router;
  protected boolean dirty = false;

  public ShortestPathRouterAdapter (GraphSceneImpl<?,?> scene)
  {
    this.scene = scene;
    this.router = new ShortestPathRouter();
    router.setSpacing( 5 );
  }

  public void addNodeWidget (Widget w)
  {
    w.addDependency( new WidgetListener( w ) );
  }

  public void removeNodeWidget (Widget w)
  {
    WidgetListener widgetListener = null;
    for (Widget.Dependency dependency : w.getDependencies() )
    {
      if ( dependency instanceof WidgetListener )
      {
        widgetListener = (WidgetListener)dependency;
      }
    }

    if ( widgetListener != null )
    {
      w.removeDependency( widgetListener );
      widgetListener.cleanup();
    }
  }

  public void addPath (Path path)
  {
    System.out.println("Add Path ");
    
    //TODO - need a better solution
    //Check for overlapping path, If there are overlapping paths
    //Force path to bend. 
    //NB: Have not tested if more than one overlapped path.
    List<Path> solve = router.solve();
    for(Path existPath : solve){
      
      if(existPath.getEndPoint().equals( path.getStartPoint() )
          && existPath.getStartPoint().equals( path.getEndPoint() )){
        PointList bendPoints = new PointList();
        int x = (path.getEndPoint().x + path.getStartPoint().x) / 2; //+ 25;
        //Add an Offset to x if Path is a vertical line
        x = x == path.getEndPoint().x ? x + 25 : x ;
        
        //Add a vertical Offset if y is a horizontal line
        int y = (path.getEndPoint().y + path.getStartPoint().y) / 2;
        y = y == path.getEndPoint().y ? y + 25 : y;
        
        bendPoints.addPoint( x, y );
        //Add a new obstacle
        path.setBendPoints( bendPoints  );
      }
    }
    
    router.addPath( path );
    dirty = true;
  }
  
  public void removePath (Path path)
  {
    System.out.println("Remove Path");
    router.removePath( path );
    dirty = true;
  }

  @Override
  public List<Point> routeConnection (ConnectionWidget conn)
  {
    System.out.println("RouteConnection  " + conn);
    PathTrackingConnectionWidget pathTrackingCW = (PathTrackingConnectionWidget)conn;
    pathTrackingCW.ensurePathCurrent();

    while ( dirty )
    {
      router.solve();
      dirty = false;
      redrawConnections( conn );
    }

    return pathTrackingCW.getRoute();
  }

  public void redrawConnections (Widget source)
  {
    System.out.println("Redraw Connections " + source);
    for ( Widget widget : scene.getConnectionLayer().getChildren() )
    {
      if ( widget != source && widget instanceof PathTrackingConnectionWidget)
      {
        PathTrackingConnectionWidget conn = (PathTrackingConnectionWidget)widget;
        conn.reroute();
      }
    }
  }

  public void setDirty ()
  {
    this.dirty = true;
  }

  public class WidgetListener implements Widget.Dependency
  {
    private Widget widget;
    private Rectangle bounds;

    public Rectangle getNewBounds ()
    {
      java.awt.Rectangle newBounds = widget.getBounds();
      System.out.println("Get new bounds " + newBounds);
      if (newBounds == null )
      {
        return null;
      }
      newBounds.setLocation( widget.getLocation() );
      return ConvertUtil.awtToSwt( newBounds );
    }

    public WidgetListener (Widget widget)
    {
      this.widget = widget;
      this.bounds = getNewBounds();

      if ( bounds != null )
      {
        router.addObstacle( bounds );
      }

      dirty = true;
    }

    @Override
    public void revalidateDependency ()
    {
      System.out.println("RevalidateDependency");
      Rectangle newBounds = getNewBounds();

      if ( bounds == null )
      {
        if ( newBounds != null )
        {
          router.addObstacle( newBounds );
        }
      }
      else if ( newBounds == null )
      {
        router.removeObstacle( bounds );
      }
      else
      {
        router.updateObstacle( bounds, newBounds );
      }

      bounds = newBounds;
      dirty = true;
    }

    public void cleanup ()
    {
      System.out.println("Cleanup");
      if ( bounds != null )
      {
        router.removeObstacle( bounds );
        dirty = true;
      }
    }
  }
}