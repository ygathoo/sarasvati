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
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.netbeans.api.visual.export.SceneExporter;
import org.netbeans.api.visual.export.SceneExporter.ImageType;
import org.netbeans.api.visual.export.SceneExporter.ZoomType;
import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.visual.GraphSceneImpl;

public class SarasvatiGraphScene extends GraphSceneImpl<Node, Arc>
{
  @Override
  protected Widget widgetForNode (Node node)
  {
    return new ComponentWidget( this, node.getAdaptor( Component.class ) );
  }

  public void export (String imageFile, StringBuilder map) throws IOException
  {
    Rectangle bounds = getScene().getPreferredBounds();
    BufferedImage image = new BufferedImage( bounds.width, bounds.height, BufferedImage.TYPE_4BYTE_ABGR );

    paint( image.createGraphics() );
    ImageIO.write( image, "gif", new File( "/home/paul/tmp/image.gif" ) );
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
  }
}