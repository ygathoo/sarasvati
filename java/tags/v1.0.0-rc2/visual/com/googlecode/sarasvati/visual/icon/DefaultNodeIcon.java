package com.googlecode.sarasvati.visual.icon;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.visual.common.NodeDrawConfig;
import com.googlecode.sarasvati.visual.util.FontUtil;

public class DefaultNodeIcon extends AbstractNodeIcon
{
  protected boolean isJoin;
  protected String label;
  protected Color color;

  public DefaultNodeIcon (Node node, NodeToken token)
  {
    this.label = node.getAdaptor( String.class );
    if ( label == null )
    {
      label = node.getName();
    }
    this.isJoin = node.isJoin();
    this.color = NodeDrawConfig.getColor( token );
    redrawImage();
  }

  public DefaultNodeIcon (String label, Color color, boolean isJoin)
  {
    this.label = label;
    this.color = color;
    this.isJoin = isJoin;
    redrawImage();
  }

  @Override
  public void redrawImage (Graphics2D g)
  {
    g.setRenderingHint( RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON );

    g.setColor( color );
    g.fillOval( 0, 0, WIDTH - 1, HEIGHT - 1 );

    //g.setColor( node.isStart() ? NodeDrawConfig.START_NODE_BORDER : NodeDrawConfig.NODE_BORDER);
    g.setColor( NodeDrawConfig.NODE_BORDER);

    float[] dashes = isJoin ? new float[] { 10, 5 } : null;

    int offset = 1;

    BasicStroke stroke = new BasicStroke( 3, BasicStroke.CAP_SQUARE, BasicStroke.JOIN_MITER, 10, dashes, 0) ;
    g.setStroke( stroke );

    int width = WIDTH - ((offset << 1) + 1);
    int height = HEIGHT - ((offset<< 1) + 1);

    g.drawOval( offset, offset, width, height);
    offset += 3;

    g.setColor( Color.white );

    int padding = 2 + offset;
    int startX = padding;

    int maxWidth = getIconWidth() - (padding << 1);

    g.setFont( g.getFont().deriveFont( Font.BOLD ) );
    FontUtil.setSizedFont( g, label, 11, maxWidth );
    int strWidth = (int)Math.ceil( g.getFontMetrics().getStringBounds( label, g ).getWidth() );
    int strHeight = g.getFontMetrics().getAscent();
    int left = startX + ((maxWidth - strWidth) >> 1);
    int top = ((getIconHeight() + strHeight) >> 1);
    g.drawString( label, left, top );
  }
}