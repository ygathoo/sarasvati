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
package com.googlecode.sarasvati.editor.model;

import com.googlecode.sarasvati.xml.XmlArc;
import com.googlecode.sarasvati.xml.XmlExternal;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlExternalArcType;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class EditorGraphFactory
{
  public static EditorGraph loadFromXml (XmlProcessDefinition xmlProcDef)
  {
    EditorGraph graph = new EditorGraph();
    graph.setName( xmlProcDef.getName() );

    for ( XmlNode xmlNode : xmlProcDef.getNodes() )
    {
      EditorNode node = new EditorNode();
      node.setName(  xmlNode.getName() );
      node.setType( xmlNode.getType() );
      node.setJoin( xmlNode.isJoin() );
      node.setStart( xmlNode.isStart() );
      node.setGuard( xmlNode.getGuard() );

      graph.addMember( node );
    }

    for ( XmlExternal xmlExternal : xmlProcDef.getExternals() )
    {
      EditorExternal external = new EditorExternal();
      external.setName( xmlExternal.getName() );
      external.setGraphName( xmlExternal.getProcessDefinition() );
      graph.addMember( external );
    }

    for ( XmlNode xmlNode : xmlProcDef.getNodes() )
    {
      for ( XmlArc xmlArc : xmlNode.getArcs() )
      {
        graph.addArc( xmlNode.getName(), xmlArc.getTo(), xmlArc.getName() );
      }

      for ( XmlExternalArc xmlExternal : xmlNode.getExternalArcs() )
      {
        if ( xmlExternal.getType() == XmlExternalArcType.OUT )
        {
          graph.addArc( xmlExternal.getExternal(), xmlExternal.getNode(), xmlExternal.getName() );
        }
        else
        {
          graph.addArc( xmlExternal.getNode(), xmlExternal.getExternal(), xmlExternal.getName() );
        }
      }
    }

    return graph;
  }
}
