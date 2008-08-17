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

package com.googlecode.sarasvati.load;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.util.SvUtil;
import com.googlecode.sarasvati.xml.XmlArc;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlExternalArcType;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlWorkflow;
import com.googlecode.sarasvati.xml.XmlWorkflowResolver;

/**
 * Given a {@link GraphFactory} to construct the {@link Graph} parts and
 * a {@link GraphRepository}, the GraphLoader will load XML process
 * definitions into that repository.
 *
 * This class is *not* thread safe
 *
 * @author Paul Lorenz
 */
public class GraphLoader<G extends Graph>
{
  protected Map<String,Map<String,Node>> instanceCache = null;
  protected Map<String,Node>             nodeCache     = null;

  protected GraphFactory<G> factory;
  protected GraphRepository<G> repository;
  protected G graph;

  public GraphLoader (GraphFactory<G> factory, GraphRepository<G> repository)
  {
    this.factory = factory;
    this.repository = repository;
  }

  protected Graph getGraph ()
  {
    return graph;
  }

  protected void importNodes (XmlWorkflow xmlDef)
    throws LoadException
  {
    for ( XmlNode xmlNode : xmlDef.getNodes() )
    {
      String nodeName = xmlNode.getName();

      if ( nodeCache.containsKey( nodeName ) )
      {
        throw new LoadException( "Node name '" + nodeName + "' is not unique in workflow: " + graph.getName() );
      }

      String type = xmlNode.getType();

      Node newNode = factory.newNode( graph, nodeName,
                                      type == null ? "node" : type,
                                      xmlNode.isJoin(),
                                      xmlNode.isStart(),
                                      xmlNode.getGuard(),
                                      xmlNode.getCustom() );
      nodeCache.put( nodeName, newNode );
    }
  }

  protected void importArcs (XmlWorkflow xmlDef) throws LoadException
  {
    for (XmlNode xmlNode : xmlDef.getNodes())
    {
      for ( XmlArc xmlArc : xmlNode.getArcs() )
      {
        Node startNode = nodeCache.get( xmlNode.getName() );
        Node endNode   = nodeCache.get( xmlArc.getTo() );

        if ( endNode == null )
        {
          throw new LoadException( "Arc in node '" + xmlNode.getName() + "' points to non-existent node '" + xmlArc.getTo() + "'" );
        }

        factory.newArc( graph, startNode, endNode, SvUtil.isBlankOrNull( xmlArc.getName() ) ? Arc.DEFAULT_ARC : xmlArc.getName() );
      }
    }
  }

  protected String getInstanceKey (XmlExternalArc externalArc)
  {
    return externalArc.getExternal() + ":" + externalArc.getInstance();
  }

  protected Node getExternalNode (XmlExternalArc externalArc) throws LoadException
  {
    Map<String,Node> instance = instanceCache.get( getInstanceKey( externalArc ) );

    if (instance == null)
    {
      instance = importInstance( externalArc.getExternal(), externalArc.getInstance() );
      instanceCache.put( getInstanceKey( externalArc ), instance );
    }

    return instance.get( externalArc.getNodeName() );
  }

  protected void importExternalArcs (XmlWorkflow xmlDef) throws LoadException
  {
    for ( XmlNode xmlNode : xmlDef.getNodes() )
    {
      for ( XmlExternalArc externalArc : xmlNode.getExternalArcs() )
      {
        Node localNode = nodeCache.get( xmlNode.getName() );
        Node extNode = getExternalNode( externalArc );

        if ( extNode == null )
        {
          throw new LoadException( "External arc in node '" + xmlNode.getName() +
                                     "' points to non-existent node '" + externalArc.getNodeName() + "'" +
                                     " in process definition '" + externalArc.getExternal() + "'" );
        }

        String arcName = SvUtil.isBlankOrNull( externalArc.getName() ) ? Arc.DEFAULT_ARC : externalArc.getName();

        if ( externalArc.getType() == XmlExternalArcType.OUT )
        {
          factory.newArc( graph, localNode, extNode, arcName );
        }
        else
        {
          factory.newArc( graph, extNode, localNode, arcName );
        }
      }
    }
  }

  protected Map<String,Node> importInstance (String externalName, String instanceName)
      throws LoadException
  {
    Map<String, Node> nodeMap = new HashMap<String, Node>();
    Graph instanceGraph = repository.getLatestGraph( externalName );

    if ( instanceGraph == null )
    {
      throw new LoadException( "Referenced external '" + externalName + "' not found in database" );
    }

    Map<Node,Node> lookupMap = new HashMap<Node, Node>();

    for ( Node node : instanceGraph.getNodes() )
    {
      Node newNode = factory.importNode( graph, node, instanceName);

      lookupMap.put( node, newNode );
      if ( !node.isExternal() )
      {
        nodeMap.put( node.getName(), newNode );
      }
    }

    for ( Arc arc : instanceGraph.getArcs() )
    {
      Node startNode = lookupMap.get( arc.getStartNode() );
      Node endNode = lookupMap.get( arc.getEndNode() );
      factory.newArc( graph, startNode, endNode, arc.getName() );
    }

    return nodeMap;
  }

  public void loadDefinition (XmlWorkflow xmlDef)
    throws LoadException
  {
    instanceCache = new HashMap<String, Map<String,Node>>();
    nodeCache     = new HashMap<String, Node>();

    Graph latest = repository.getLatestGraph( xmlDef.getName() );

    int version = latest == null ? 1 : latest.getVersion() + 1;

    graph = factory.newGraph( xmlDef.getName(), version );
    repository.addGraph( graph );
    importNodes( xmlDef );
    importArcs( xmlDef );
    importExternalArcs( xmlDef );
  }

  public void loadWithDependencies (String name, XmlWorkflowResolver resolver)
    throws JAXBException, LoadException
  {
    loadWithDependencies( name, resolver, new ArrayList<String>() );
  }

  private void loadWithDependencies (String name, XmlWorkflowResolver resolver, List<String> stack)
      throws JAXBException, LoadException
  {
    stack.add( name );
    XmlWorkflow xmlDef = resolver.resolve( name );

    for ( XmlNode node : xmlDef.getNodes() )
    {
      for (XmlExternalArc extArc : node.getExternalArcs() )
      {
        String extName = extArc.getExternal();
        if ( stack.contains( extName ) )
        {
          throw new LoadException( "Process definition '" + name + "' contains an illegal recursive reference to '" + extName + "'" );
        }

        if ( !isLoaded( extName ) )
        {
          loadWithDependencies( extName, resolver, stack );
        }
      }
    }

    stack.remove( stack.size() - 1 );

    loadDefinition( xmlDef );
  }

  public boolean isLoaded(String name)
  {
    return null != repository.getLatestGraph( name );
  }
}