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

package com.googlecode.sarasvati.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

@XmlAccessorType(XmlAccessType.FIELD)
public class XmlExternal
{
  @XmlAttribute (name="processDefinition", required=true)
  protected String processDefinition;

  @XmlAttribute (name="name", required=true)
  protected String name;

  @XmlAttribute (name="x", required=false)
  protected String x;

  @XmlAttribute (name="y", required=false)
  protected String y;

  public String getProcessDefinition()
  {
    return processDefinition;
  }

  public void setProcessDefinition( String processDefinition )
  {
    this.processDefinition = processDefinition;
  }

  public String getName()
  {
    return name;
  }

  public void setName( String name )
  {
    this.name = name;
  }

  public String getX()
  {
    return x;
  }

  public void setX( String x )
  {
    this.x = x;
  }

  public String getY()
  {
    return y;
  }

  public void setY( String y )
  {
    this.y = y;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder();
    buf.append( "<external name=\"" );
    buf.append( name );
    buf.append( "\" processDefinition=\"" );
    buf.append( processDefinition );

    if ( x != null )
    {
      buf.append( "\" x=\"" );
      buf.append( x );
    }

    if ( y != null )
    {
      buf.append( "\" y=\"" );
      buf.append( y );
    }

    buf.append( "\"/>\n" );
    return buf.toString();
  }
}
