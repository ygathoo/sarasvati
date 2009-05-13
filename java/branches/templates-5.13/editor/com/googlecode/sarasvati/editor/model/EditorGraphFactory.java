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

import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.xml.XmlArc;
import com.googlecode.sarasvati.xml.XmlExternal;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlJoinType;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class EditorGraphFactory
{
  public static EditorGraph loadFromXml (XmlProcessDefinition xmlProcDef)
  {
    EditorGraph graph = new EditorGraph();
    graph.setName( xmlProcDef.getName() );

    Map<String, EditorNode> nodeMap = new HashMap<String, EditorNode>();
    Map<String, EditorExternal> externalMap = new HashMap<String, EditorExternal>();

    for ( XmlNode xmlNode : xmlProcDef.getNodes() )
    {
      EditorNode node = new EditorNode();
      node.setName( xmlNode.getName() );
      node.setType( xmlNode.getType() );
      node.setJoinType( xmlNode.getJoinType().getJoinType() );
      node.setStart( xmlNode.isStart() );
      node.setGuard( xmlNode.getGuard() );

      if ( xmlNode.getX() != null && xmlNode.getY() != null )
      {
        node.setX( xmlNode.getX() );
        node.setY( xmlNode.getY() );
      }

      graph.addNode( node );
      nodeMap.put( node.getName(), node );
    }

    for ( XmlExternal xmlExternal : xmlProcDef.getExternals() )
    {
      EditorExternal external = new EditorExternal();
      external.setName( xmlExternal.getName() );
      external.setGraphName( xmlExternal.getProcessDefinition() );

      if ( xmlExternal.getX() != null && xmlExternal.getY() != null )
      {
        external.setX( xmlExternal.getX() );
        external.setY( xmlExternal.getY() );
      }

      graph.addExternal( external );
      externalMap.put( external.getName(), external );
    }

    for ( XmlNode xmlNode : xmlProcDef.getNodes() )
    {
      for ( XmlArc xmlArc : xmlNode.getArcs() )
      {
        EditorGraphMember startMember = nodeMap.get( xmlNode.getName() );

        EditorArc arc = new EditorArc();
        arc.setStart( startMember );
        arc.setLabel( xmlArc.getName() );

        if ( xmlArc.isToExternal() )
        {
          arc.setEnd( nodeMap.get( xmlArc.getExternal() ) );
          arc.setExternalEnd( xmlArc.getTo() );
        }
        else
        {
          arc.setEnd( nodeMap.get( xmlArc.getTo() ) );
        }

        graph.addArc( arc );
      }
    }

    for ( XmlExternal xmlExternal : xmlProcDef.getExternals() )
    {
      EditorGraphMember startMember = nodeMap.get( xmlExternal.getName() );

      for ( XmlExternalArc xmlExternalArc : xmlExternal.getExternalArcs() )
      {
        EditorArc arc = new EditorArc();
        arc.setStart( startMember );
        arc.setExternalStart( xmlExternalArc.getFrom() );
        arc.setLabel( xmlExternalArc.getName() );

        if ( xmlExternalArc.isToExternal() )
        {
          arc.setEnd( nodeMap.get( xmlExternalArc.getExternal() ) );
          arc.setExternalEnd( xmlExternalArc.getTo() );
        }
        else
        {
          arc.setEnd( nodeMap.get( xmlExternalArc.getTo() ) );
        }

        graph.addArc( arc );
      }
    }

    return graph;
  }

  public static XmlProcessDefinition exportToXml (EditorGraph graph)
  {
    XmlProcessDefinition xmlProcDef = new XmlProcessDefinition();
    xmlProcDef.setName( graph.getName() );

    Map<EditorNode,XmlNode> nodeMap = new HashMap<EditorNode, XmlNode>();
    Map<EditorExternal,XmlExternal> externalMap = new HashMap<EditorExternal, XmlExternal>();

    for ( EditorNode node : graph.getNodes() )
    {
      XmlNode xmlNode = new XmlNode();
      xmlNode.setName( node.getName() );
      xmlNode.setType( node.getType() );
      xmlNode.setGuard( node.getGuard() );
      xmlNode.setJoinType( XmlJoinType.getXmlJoinType( node.getJoinType() ) );
      xmlNode.setStart( node.isStart() );
      xmlNode.setX( node.getX() );
      xmlNode.setY( node.getY() );

      xmlProcDef.getNodes().add( xmlNode );
      nodeMap.put(  node, xmlNode );
    }

    for ( EditorExternal external : graph.getExternals() )
    {
      XmlExternal xmlExternal = new XmlExternal();
      xmlExternal.setName( external.getName() );
      xmlExternal.setProcessDefinition( external.getGraphName() );
      xmlExternal.setX( external.getX() );
      xmlExternal.setY( external.getY() );

      xmlProcDef.getExternals().add( xmlExternal );
      externalMap.put( external, xmlExternal );
    }

    for ( EditorArc arc : graph.getArcs() )
    {
//      if ( arc.isExternalOutArc() )
//      {
//        XmlNode node = nodeMap.get( arc.getStart() );
//        XmlExternalArc xmlExternalArc = new XmlExternalArc();
//        xmlExternalArc.setExternal( arc.getEnd().getName() );
//        xmlExternalArc.setName( arc.getLabel() );
//        xmlExternalArc.setTo( "test" ); // TODO: Fix, once we have UI for selecting node
//        xmlExternalArc.setType( XmlExternalArcType.OUT );
//
//        node.getExternalArcs().add( xmlExternalArc );
//      }
//      else if ( arc.isExternalInArc() )
//      {
//        XmlNode node = nodeMap.get( arc.getEnd() );
//        XmlExternalArc xmlExternalArc = new XmlExternalArc();
//        xmlExternalArc.setExternal( arc.getStart().getName() );
//        xmlExternalArc.setName( arc.getLabel() );
//        xmlExternalArc.setNode( "test" ); // TODO: Fix, once we have UI for selecting node
//        xmlExternalArc.setType( XmlExternalArcType.IN );
//
//        node.getExternalArcs().add( xmlExternalArc );
//      }
//      else
//      {
        XmlNode node = nodeMap.get( arc.getStart() );
        XmlArc xmlArc = new XmlArc();
        xmlArc.setName( arc.getLabel() );
        xmlArc.setTo( arc.getEnd().getName() );
        node.getArcs().add( xmlArc );
//      }
    }

    return xmlProcDef;
  }
}