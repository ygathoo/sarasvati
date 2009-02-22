package com.googlecode.sarasvati.editor.model;

import com.googlecode.sarasvati.xml.XmlArc;
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

    for ( XmlNode xmlNode : xmlProcDef.getNodes() )
    {
      for ( XmlArc xmlArc : xmlNode.getArcs() )
      {
        graph.addArc( xmlNode.getName(), xmlArc.getTo(), xmlArc.getName() );
      }

      for ( XmlExternalArc xmlExternal : xmlNode.getExternalArcs() )
      {
        if ( !graph.hasMember( xmlExternal.getInstance() ) )
        {
          EditorExternal external = new EditorExternal();
          external.setName( xmlExternal.getInstance() );
          external.setGraphName( external.getGraphName() );
          graph.addMember( external );
        }

        if ( xmlExternal.getType() == XmlExternalArcType.OUT )
        {
          graph.addArc( xmlExternal.getInstance(), xmlExternal.getNodeName(), xmlExternal.getName() );
        }
        else
        {
          graph.addArc( xmlExternal.getNodeName(), xmlExternal.getInstance(), xmlExternal.getName() );
        }
      }
    }

    return graph;
  }
}
