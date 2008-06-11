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

package org.codemonk.wf.xml;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement (name="workflow",namespace="http://sarasvati.googlecode.com/workflow/")
@XmlAccessorType(XmlAccessType.FIELD)
public class XmlWorkflow
{
  @XmlAttribute (name="name", required=true)
  protected String name;

  @XmlElement (name="node", namespace="http://sarasvati.googlecode.com/workflow/")
  protected List<XmlNode> nodes = new ArrayList<XmlNode>();

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public List<XmlNode> getNodes ()
  {
    return nodes;
  }

  public void setNodes (List<XmlNode> nodes)
  {
    this.nodes = nodes;
  }

  @Override
  public String toString ()
  {
    StringBuilder buf = new StringBuilder ();
    buf.append( "<workflow name=\"" );
    buf.append( getName() );
    buf.append( "\">\n" );

    for ( XmlNode node : nodes )
    {
      buf.append( node );
      buf.append( "\n" );
    }

    buf.append( "</workflow>" );
    return buf.toString();
  }
}
