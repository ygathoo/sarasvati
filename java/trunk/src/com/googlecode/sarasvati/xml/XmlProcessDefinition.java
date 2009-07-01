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
import javax.xml.bind.annotation.XmlRootElement;

import com.googlecode.sarasvati.load.definition.ExternalDefinition;
import com.googlecode.sarasvati.load.definition.NodeDefinition;
import com.googlecode.sarasvati.load.definition.ProcessDefinition;

@XmlRootElement(name = "process-definition")
@XmlAccessorType(XmlAccessType.FIELD)
public class XmlProcessDefinition implements ProcessDefinition
{
  @XmlAttribute(name = "name", required = true)
  protected String            name;

  @XmlElement(name = "node")
  protected List<XmlNode>     nodes     = new ArrayList<XmlNode>();

  @XmlElement(name = "external")
  protected List<XmlExternal> externals = new ArrayList<XmlExternal>();

  @Override
  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  @Override
  public List<XmlNode> getNodes ()
  {
    return nodes;
  }

  public void setNodes (List<XmlNode> nodes)
  {
    this.nodes = nodes;
  }

  @Override
  public List<XmlExternal> getExternals ()
  {
    return externals;
  }

  public void setExternals (List<XmlExternal> externals)
  {
    this.externals = externals;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder();
    buf.append( "<workflow name=\"" );
    buf.append( getName() );
    buf.append( "\">\n" );

    for (NodeDefinition node : nodes)
    {
      buf.append( node );
      buf.append( "\n" );
    }

    for (ExternalDefinition external : externals)
    {
      buf.append( external );
      buf.append( "\n" );
    }

    buf.append( "</workflow>" );
    return buf.toString();
  }
}
