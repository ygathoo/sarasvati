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

import java.awt.Color;
import java.awt.Paint;

import org.apache.commons.collections15.Transformer;

import com.googlecode.sarasvati.hib.HibNodeRef;

public class NodeColorTransformer implements Transformer<HibNodeRef, Paint>
{
  protected Color startColor = new Color( 102, 152, 102 );
  protected Color nodeColor  = new Color( 102, 102, 152 );

  @Override
  public Paint transform( HibNodeRef nodeRef )
  {
    if ( "start".equals( nodeRef.getType() ) )
    {
      return startColor;
    }
    else if ( "task".equals(  nodeRef.getType() ) )
    {
      return Color.blue;
    }

    return nodeColor;
  }
}
