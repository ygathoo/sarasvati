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
package com.googlecode.sarasvati.visual;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Node;

/**
 * Provides default implementations for methods in the
 * {@link GraphToImageMap} interface. All methods return
 * null unless overridden.
 *
 * @author Paul Lorenz
 */
public class GraphToImageMapAdapter implements GraphToImageMap
{
  /**
   * Returns null unless overridden.
   */
  @Override
  public String hoverForArc (Arc arc)
  {
    return null;
  }

  /**
   * Returns null unless overridden.
   */
  @Override
  public String hoverForNode (Node node)
  {
    return null;
  }

  /**
   * Returns null unless overridden.
   */
  @Override
  public String hrefForArc (Arc arc)
  {
    return null;
  }

  /**
   * Returns null unless overridden.
   */
  @Override
  public String hrefForNode (Node node)
  {
    return null;
  }
}
