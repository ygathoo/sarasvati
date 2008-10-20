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
package com.googlecode.sarasvati.visual.graph;

import java.awt.Component;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.imageio.ImageIO;

import org.netbeans.api.visual.export.SceneExporter;
import org.netbeans.api.visual.export.WidgetPolygonalCoordinates;
import org.netbeans.api.visual.export.SceneExporter.ImageType;
import org.netbeans.api.visual.export.SceneExporter.ZoomType;
import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.adapter.Function;
import com.googlecode.sarasvati.visual.ConvertUtil;
import com.googlecode.sarasvati.visual.GraphSceneImpl;

public class SarasvatiGraphScene extends GraphSceneImpl<Node, Arc>
{
  @Override
  protected Widget widgetForNode (Node node)
  {
    return new ComponentWidget( this, node.getAdaptor( Component.class ) );
  }

  public void export (String imageFile, StringBuilder buf, Function<String, Widget> hrefMapper, Function<String, Widget> titleMapper )
    throws IOException
  {
    List<WidgetPolygonalCoordinates> coords =
      SceneExporter.createImageMap( getScene(),
                                    new File( imageFile),
                                    ImageType.PNG,
                                    ZoomType.ACTUAL_SIZE,
                                    false,
                                    false,
                                    100,
                                    this.getView().getWidth(),
                                    this.getView().getHeight(),
                                    0 );

    for ( WidgetPolygonalCoordinates coord : coords )
    {
      buf.append( "<area shape=\"poly\" coords=\"" );
      ConvertUtil.appendPolygon( coord.getPolygon(), buf );
      buf.append( "\" " );
      buf.append( hrefMapper.apply( coord.getWidget() ) );
      buf.append( " " );
      buf.append( titleMapper.apply( coord.getWidget() ) );
      buf.append( ">\n" );
    }

    Rectangle bounds = getScene().getPreferredBounds();
    BufferedImage image = new BufferedImage( bounds.width, bounds.height, BufferedImage.TYPE_4BYTE_ABGR );

    Graphics2D g = image.createGraphics();
    g.setRenderingHint( RenderingHints.KEY_ANTIALIASING, // Anti-alias!
                        RenderingHints.VALUE_ANTIALIAS_ON );

    paint( g );
    ImageIO.write( image, "gif", new File( "/home/paul/tmp/image.gif" ) );
  }
}