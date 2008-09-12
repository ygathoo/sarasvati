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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class XmlNode
{
  @XmlAttribute (name="name", required=true)
  protected String name;

  @XmlAttribute (name="isJoin", required=false)
  protected Boolean join;

  @XmlAttribute (name="type", required=false)
  protected String type;

  @XmlAttribute (name="isStart", required=false)
  protected Boolean start;

  @XmlElement (name="guard", required=false)
  protected String guard;

  @XmlElement (name="arc", required=false)
  protected List<XmlArc> arcs = new ArrayList<XmlArc>();

  @XmlElement (name="externalArc", required=false)
  protected List<XmlExternalArc> externalArcs = new ArrayList<XmlExternalArc>();

  @XmlAnyElement (lax=true)
  protected List<Object> custom;

  public String getName()
  {
    return name;
  }

  public void setName( String name )
  {
    this.name = name;
  }

  public boolean isJoin ()
  {
    return join == null ? false : join;
  }

  public Boolean getJoin()
  {
    return join;
  }

  public void setJoin( Boolean join )
  {
    this.join = join;
  }

  public String getType()
  {
    return type;
  }

  public void setType( String type )
  {
    this.type = type;
  }

  public boolean isStart ()
  {
    return start == null ? false : start;
  }

  public Boolean getStart()
  {
    return start;
  }

  public void setStart( Boolean start )
  {
    this.start = start;
  }

  public String getGuard()
  {
    return guard;
  }

  public void setGuard( String guard )
  {
    this.guard = guard;
  }

  public List<XmlArc> getArcs()
  {
    return arcs;
  }

  public void setArcs( List<XmlArc> arcs )
  {
    this.arcs = arcs;
  }

  public List<XmlExternalArc> getExternalArcs()
  {
    return externalArcs;
  }

  public void setExternalArcs( List<XmlExternalArc> externalArcs )
  {
    this.externalArcs = externalArcs;
  }

  public List<Object> getCustom ()
  {
    return custom;
  }

  public void setCustom (List<Object> custom)
  {
    this.custom = custom;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder();
    buf.append( "<node name=\"" );
    buf.append( name );
    buf.append( "\" isJoin=\"" );
    buf.append( isJoin() );
    buf.append( "\" type=\"" );
    buf.append( type );
    buf.append( "\" isStart=\"" );
    buf.append( isStart() );
    buf.append( "\">\n" );

    for ( XmlArc arc : arcs )
    {
      buf.append( arc );
      buf.append( "\n" );
    }

    for ( XmlExternalArc arc : externalArcs )
    {
      buf.append( arc );
      buf.append( "\n" );
    }

    if (custom != null )
    {
      buf.append( custom );
      buf.append( "\n" );
    }

    buf.append( "</node>" );
    return buf.toString();
  }
}
