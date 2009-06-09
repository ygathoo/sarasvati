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
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

@XmlAccessorType(XmlAccessType.FIELD)
public class XmlExternal
{
  @XmlAttribute(name = "processDefinition", required = true)
  protected String  processDefinition;

  @XmlAttribute(name = "name", required = true)
  protected String  name;

  @XmlAttribute(name = "x", required = false)
  protected Integer x;

  @XmlAttribute(name = "y", required = false)
  protected Integer y;

  @XmlElement(name = "arc", required = false)
  protected List<XmlExternalArc> externalArcs = new ArrayList<XmlExternalArc>();

  public String getProcessDefinition ()
  {
    return processDefinition;
  }

  public void setProcessDefinition (String processDefinition)
  {
    this.processDefinition = processDefinition;
  }

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public Integer getX ()
  {
    return x;
  }

  public void setX (Integer x)
  {
    this.x = x;
  }

  public Integer getY ()
  {
    return y;
  }

  public void setY (Integer y)
  {
    this.y = y;
  }

  public List<XmlExternalArc> getExternalArcs()
  {
    return externalArcs;
  }

  public void setExternalArcs( List<XmlExternalArc> externalArcs )
  {
    this.externalArcs = externalArcs;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder();
    buf.append( "<external name=\"" );
    buf.append( name );
    buf.append( "\" processDefinition=\"" );
    buf.append( processDefinition );

    if (x != null)
    {
      buf.append( "\" x=\"" );
      buf.append( x );
    }

    if (y != null)
    {
      buf.append( "\" y=\"" );
      buf.append( y );
    }

    buf.append( ">\n" );

    for (XmlExternalArc arc : externalArcs)
    {
      buf.append( arc );
      buf.append( "\n" );
    }

    buf.append( "</external>\n" );
    return buf.toString();
  }
}
