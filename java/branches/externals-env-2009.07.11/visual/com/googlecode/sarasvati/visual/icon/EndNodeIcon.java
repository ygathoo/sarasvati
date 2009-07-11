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

    Copyright 2009 Cheong Chung Onn
*/
package com.googlecode.sarasvati.visual.icon;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Ellipse2D;

public class EndNodeIcon extends AbstractNodeIcon
{
  public EndNodeIcon ()
  {
    redrawImage();
  }

  @Override
  public void redrawImage (Graphics2D g)
  {
    double diameter = Math.min( WIDTH, HEIGHT );
    double outerCircle = diameter * 0.7;
    double innerCircle = diameter * 0.5;

    g.setColor( Color.BLACK.brighter() );
    BasicStroke stroke = new BasicStroke( 3, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10,
        null, 0 );
    g.setStroke( stroke );
    int offset = 1;
    g.translate(( WIDTH - outerCircle ) / 2 , offset );
    g.draw( new Ellipse2D.Double( 0, 0, outerCircle, outerCircle ) );
    g.translate( (outerCircle - innerCircle) / 2  , ( outerCircle - innerCircle ) / 2 );
    g.fill( new Ellipse2D.Double( 0, 0, innerCircle, innerCircle ) );
    g.dispose();

  }

}
