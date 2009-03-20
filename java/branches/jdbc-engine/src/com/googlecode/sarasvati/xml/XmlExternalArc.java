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
public class XmlExternalArc
{
  @XmlAttribute(name = "external", required = true)
  protected String             external;

  @XmlAttribute(name = "instance", required = true)
  protected String             instance;

  @XmlAttribute(name = "nodeName", required = true)
  protected String             nodeName;

  @XmlAttribute(name = "name", required = false)
  protected String             name;

  @XmlAttribute(name = "type", required = true)
  protected XmlExternalArcType type;

  public String getExternal ()
  {
    return external;
  }

  public void setExternal (String external)
  {
    this.external = external;
  }

  public String getInstance ()
  {
    return instance;
  }

  public void setInstance (String instance)
  {
    this.instance = instance;
  }

  public String getNodeName ()
  {
    return nodeName;
  }

  public void setNodeName (String nodeName)
  {
    this.nodeName = nodeName;
  }

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public XmlExternalArcType getType ()
  {
    return type;
  }

  public void setType (XmlExternalArcType type)
  {
    this.type = type;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder ();
    buf.append( "<externalArc external=\"" );
    buf.append( external );
    buf.append( "\" instance=\"" );
    buf.append( instance );
    buf.append( "\" nodeName=\"" );
    buf.append( nodeName );

    if ( name != null )
    {
      buf.append( "\" name=\"" );
      buf.append( name );
    }

    buf.append( "\" type=\"" );
    buf.append( type.name().toLowerCase() );

    buf.append( "\"/>" );
    return buf.toString();
  }
}
