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

import com.googlecode.sarasvati.util.SvUtil;

@XmlAccessorType(XmlAccessType.FIELD)
public class XmlExternalArc
{
  @XmlAttribute(name = "from", required = true)
  protected String from;

  @XmlAttribute(name = "external", required = false)
  protected String external;

  @XmlAttribute(name = "to", required = true)
  protected String to;

  @XmlAttribute(name = "name", required = false)
  protected String name;

  public String getFrom()
  {
    return from;
  }

  public void setFrom( String from )
  {
    this.from = from;
  }

  public String getExternal()
  {
    return external;
  }

  public void setExternal( String external )
  {
    this.external = external;
  }

  public String getTo()
  {
    return to;
  }

  public void setTo( String to )
  {
    this.to = to;
  }

  public String getName()
  {
    return name;
  }

  public void setName( String name )
  {
    this.name = name;
  }

  public boolean isToExternal ()
  {
    return !SvUtil.isBlankOrNull( external );
  }

  @Override
  public String toString()
  {
    StringBuilder buf = new StringBuilder();
    buf.append( "<arc from=\"" );
    buf.append( from );
    buf.append( "\"" );

    if ( external != null )
    {
      buf.append( "external=\"" );
      buf.append( external );
      buf.append( "\"" );
    }
    buf.append( " to=\"" );
    buf.append( to );

    if (name != null)
    {
      buf.append( "\" name=\"" );
      buf.append( name );
      buf.append( "\"" );
    }

    buf.append( "/>" );
    return buf.toString();
  }
}
