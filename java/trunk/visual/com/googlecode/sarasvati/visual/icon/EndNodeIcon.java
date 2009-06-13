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
