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
/**
 * Created on May 7, 2008
 */
package com.googlecode.sarasvati.visual;

import java.awt.Color;

public class NodeDrawConfig
{
  private static int maxNodeRadius = 50;
  private static int verticalNodeSpacing   = 20;
  private static int horizontalNodeSpacing = 70;
  private static boolean vertical = true;

  public static final Color START_NODE_BORDER = new Color( 0, 128, 0 );
  public static final Color NODE_BORDER       = Color.black; // new Color( 0, 0, 200 );

  public static final Color NODE_BACKGROUND   = new Color( 255, 255, 225 );

  public static int getMaxNodeRadius ()
  {
    return maxNodeRadius;
  }

  public static void setMaxNodeRadius (int newMaxNodeRadius)
  {
    maxNodeRadius = newMaxNodeRadius;
  }

  public static int getVerticalNodeSpacing ()
  {
    return verticalNodeSpacing;
  }

  public static void setVerticalNodeSpacing (int newVerticalNodeSpacing)
  {
    verticalNodeSpacing = newVerticalNodeSpacing;
  }

  public static int getHorizontalNodeSpacing ()
  {
    return horizontalNodeSpacing;
  }

  public static void sethorizontalNodeSpacing (int newHorizontalNodeSpacing)
  {
    horizontalNodeSpacing = newHorizontalNodeSpacing;
  }

  public static void setVertical (boolean isVertical)
  {
    vertical = isVertical;
  }

  public static boolean isVertical ()
  {
    return vertical;
  }
}