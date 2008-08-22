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

public class NodeDrawConfig
{
  private static int maxNodeRadius = 25;
  private static int nodeSpacing   = 50;
  private static int anchorSize    = 4;

  public static int getMaxNodeRadius ()
  {
    return maxNodeRadius;
  }

  public static void setMaxNodeRadius (int newMaxNodeRadius)
  {
    maxNodeRadius = newMaxNodeRadius;
  }

  public static int getNodeSpacing ()
  {
    return nodeSpacing;
  }

  public static void setNodeSpacing (int newNodeSpacing)
  {
    nodeSpacing = newNodeSpacing;
  }

  public static int getAnchorSize()
  {
    return anchorSize;
  }

  public static void setAnchorSize( int anchorSize )
  {
    NodeDrawConfig.anchorSize = anchorSize;
  }


}