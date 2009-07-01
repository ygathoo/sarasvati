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

    Copyright 2008-2009 Paul Lorenz
*/
package com.googlecode.sarasvati.editor.model;

import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.load.properties.DOMToObjectLoadHelper;
import com.googlecode.sarasvati.util.SvUtil;
import com.googlecode.sarasvati.xml.XmlArc;
import com.googlecode.sarasvati.xml.XmlCustom;
import com.googlecode.sarasvati.xml.XmlExternal;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlJoinType;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class EditorGraphFactory
{
  public static EditorGraph loadFromXml (XmlProcessDefinition xmlProcDef) throws LoadException
  {
    EditorGraph graph = new EditorGraph();
    graph.setName( xmlProcDef.getName() );

    Map<String, EditorNode> nodeMap = new HashMap<String, EditorNode>();
    Map<String, EditorExternal> externalMap = new HashMap<String, EditorExternal>();

    for ( XmlNode xmlNode : xmlProcDef.getNodes() )
    {
      XmlCustom custom = xmlNode.getCustom();
      Map<String,String> customProperties = new LinkedHashMap<String,String>();
      if ( custom != null && custom.getCustom() != null )
      {
        DOMToObjectLoadHelper.loadCustomIntoMap( custom.getCustom(), customProperties );
      }

      NodeState nodeState = new NodeState( xmlNode.getName(),
                                           xmlNode.getType(),
                                           xmlNode.getJoinType().getJoinType(),
                                           xmlNode.getJoinParam(),
                                           xmlNode.isStart(),
                                           xmlNode.getGuard(),
                                           customProperties );
      EditorNode node = new EditorNode( nodeState );

      if ( xmlNode.getX() != null && xmlNode.getY() != null )
      {
        node.setX( xmlNode.getX() );
        node.setY( xmlNode.getY() );
      }

      graph.addNode( node );
      nodeMap.put( node.getState().getName(), node );
    }

    for ( XmlExternal xmlExternal : xmlProcDef.getExternals() )
    {
      ExternalState externalState = new ExternalState( xmlExternal.getName(), xmlExternal.getProcessDefinition() );
      EditorExternal external = new EditorExternal( externalState );

      if ( xmlExternal.getX() != null && xmlExternal.getY() != null )
      {
        external.setX( xmlExternal.getX() );
        external.setY( xmlExternal.getY() );
      }

      graph.addExternal( external );
      externalMap.put( external.getState().getName(), external );
    }

    for ( XmlNode xmlNode : xmlProcDef.getNodes() )
    {
      for ( XmlArc xmlArc : xmlNode.getArcs() )
      {
        String externalEnd = null;
        EditorGraphMember<?> startMember = nodeMap.get( xmlNode.getName() );
        EditorGraphMember<?> endMember = null;

        if ( xmlArc.isToExternal() )
        {
          endMember = nodeMap.get( xmlArc.getExternal() );
          externalEnd = xmlArc.getTo();
        }
        else
        {
          endMember = nodeMap.get( xmlArc.getTo() );
        }

        ArcState state = new ArcState( xmlArc.getName(), null, externalEnd );
        EditorArc arc = new EditorArc( state, startMember, endMember );

        graph.addArc( arc );
      }
    }

    for ( XmlExternal xmlExternal : xmlProcDef.getExternals() )
    {
      EditorGraphMember<?> startMember = nodeMap.get( xmlExternal.getName() );

      for ( XmlExternalArc xmlExternalArc : xmlExternal.getExternalArcs() )
      {
        EditorGraphMember<?> endMember = null;
        String externalEnd = null;

        if ( xmlExternalArc.isToExternal() )
        {
          endMember = nodeMap.get( xmlExternalArc.getExternal() );
          externalEnd = xmlExternalArc.getTo();
        }
        else
        {
          endMember = nodeMap.get( xmlExternalArc.getTo() );
        }

        ArcState state = new ArcState( xmlExternalArc.getName(), xmlExternalArc.getFrom(), externalEnd );
        EditorArc arc = new EditorArc( state, startMember, endMember );

        graph.addArc( arc );
      }
    }

    return graph;
  }

  public static XmlProcessDefinition exportToXml (EditorGraph graph) throws IOException
  {
    XmlProcessDefinition xmlProcDef = new XmlProcessDefinition();
    xmlProcDef.setName( graph.getName() );

    Map<EditorNode,XmlNode> nodeMap = new HashMap<EditorNode, XmlNode>();
    Map<EditorExternal,XmlExternal> externalMap = new HashMap<EditorExternal, XmlExternal>();

    for ( EditorNode node : graph.getNodes() )
    {
      XmlNode xmlNode = new XmlNode();
      NodeState state = node.getState();
      xmlNode.setName( state.getName() );
      xmlNode.setType( state.getType() );
      xmlNode.setGuard( SvUtil.nullIfBlank( state.getGuard() ) );
      xmlNode.setJoinType( XmlJoinType.getXmlJoinType( state.getJoinType() ) );

      if ( state.isStart() )
      {
        xmlNode.setStart( true );
      }

      xmlNode.setX( node.getX() );
      xmlNode.setY( node.getY() );

      List<Object> customList = DOMToObjectLoadHelper.mapToDOM( state.getCustomProperties() );

      if ( !customList.isEmpty() )
      {
        XmlCustom custom = new XmlCustom();
        custom.setCustom( customList );
        xmlNode.setCustom( custom );
      }

      xmlProcDef.getNodes().add( xmlNode );
      nodeMap.put( node, xmlNode );
    }

    for ( EditorExternal external : graph.getExternals() )
    {
      XmlExternal xmlExternal = new XmlExternal();
      xmlExternal.setName( external.getState().getName() );
      xmlExternal.setProcessDefinition( external.getState().getGraphName() );
      xmlExternal.setX( external.getX() );
      xmlExternal.setY( external.getY() );

      xmlProcDef.getExternals().add( xmlExternal );
      externalMap.put( external, xmlExternal );
    }

    for ( EditorArc arc : graph.getArcs() )
    {
      ArcState state = arc.getState();

      if ( arc.isExternalInArc() )
      {
        XmlExternal external = externalMap.get( arc.getStart().getState().getName() );
        XmlExternalArc xmlExternalArc = new XmlExternalArc();
        xmlExternalArc.setFrom( state.getExternalStart() );
        xmlExternalArc.setName( state.getLabel() );
        if ( arc.isExternalOutArc() )
        {
          xmlExternalArc.setExternal( arc.getEnd().getName() );
          xmlExternalArc.setTo( state.getExternalEnd() );
        }
        else
        {
          xmlExternalArc.setTo( arc.getEnd().getName() );
        }
        external.getExternalArcs().add( xmlExternalArc );
      }
      else
      {
        XmlNode node = nodeMap.get( arc.getStart() );
        XmlArc xmlArc = new XmlArc();
        xmlArc.setName( state.getLabel() );

        if ( arc.isExternalOutArc() )
        {
          xmlArc.setExternal( arc.getEnd().getState().getName() );
          xmlArc.setTo( state.getExternalEnd() );
        }
        else
        {
          xmlArc.setTo( arc.getEnd().getName() );
        }
        node.getArcs().add( xmlArc );
      }
    }

    return xmlProcDef;
  }
}