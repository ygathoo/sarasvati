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

package com.googlecode.sarasvati;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

import com.googlecode.sarasvati.util.SvUtil;
import com.googlecode.sarasvati.xml.XmlArc;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlExternalArcType;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlWorkflow;
import com.googlecode.sarasvati.xml.XmlWorkflowResolver;

public abstract class BaseLoader<E extends Engine, G extends Graph> implements Loader<G>
{
  protected Map<String,Map<String,Node>> instanceCache = null;
  protected Map<String,Node>             nodeCache     = null;

  protected E engine;

  public BaseLoader (E engine)
  {
    this.engine = engine;
  }

  private G graph;

  protected G getGraph ()
  {
    return graph;
  }

  protected abstract Node createNode (String name,
                                      String type,
                                      boolean isJoin,
                                      boolean isStart,
                                      String guard,
                                      Object custom)
    throws ImportException;



  protected void importNodes (XmlWorkflow xmlDef) throws ImportException
  {
    for ( XmlNode xmlNode : xmlDef.getNodes() )
    {
      String nodeName = xmlNode.getName();

      if ( nodeCache.containsKey( nodeName ) )
      {
        throw new ImportException( "Node name '" + nodeName + "' is not unique in workflow: " + getGraph().getName() );
      }

      String type = xmlNode.getType();

      Node newNode = createNode( nodeName,
                                 type == null ? "node" : type,
                                 xmlNode.isJoin(),
                                 xmlNode.isStart(),
                                 xmlNode.getGuard(),
                                 xmlNode.getCustom() );
      nodeCache.put( nodeName, newNode );
    }
  }

  protected void importArcs (XmlWorkflow xmlDef) throws ImportException
  {
    for (XmlNode xmlNode : xmlDef.getNodes())
    {
      for ( XmlArc xmlArc : xmlNode.getArcs() )
      {
        Node startNode = nodeCache.get( xmlNode.getName() );
        Node endNode   = nodeCache.get( xmlArc.getTo() );

        if ( endNode == null )
        {
          throw new ImportException( "Arc in node '" + xmlNode.getName() + "' points to non-existent node '" + xmlArc.getTo() + "'" );
        }

        engine.getFactory().createArc( graph, startNode, endNode, SvUtil.isBlankOrNull( xmlArc.getName() ) ? Arc.DEFAULT_ARC : xmlArc.getName() );
      }
    }
  }

  protected String getInstanceKey (XmlExternalArc externalArc)
  {
    return externalArc.getExternal() + ":" + externalArc.getInstance();
  }

  protected Node getExternalNode (XmlExternalArc externalArc) throws ImportException
  {
    Map<String,Node> instance = instanceCache.get( getInstanceKey( externalArc ) );

    if (instance == null)
    {
      instance = importInstance( externalArc.getExternal(), externalArc.getInstance() );
      instanceCache.put( getInstanceKey( externalArc ), instance );
    }

    return instance.get( externalArc.getNodeName() );
  }

  protected void importExternalArcs (XmlWorkflow xmlDef) throws ImportException
  {
    for ( XmlNode xmlNode : xmlDef.getNodes() )
    {
      for ( XmlExternalArc externalArc : xmlNode.getExternalArcs() )
      {
        Node localNode = nodeCache.get( xmlNode.getName() );
        Node extNode = getExternalNode( externalArc );

        if ( extNode == null )
        {
          throw new ImportException( "External arc in node '" + xmlNode.getName() +
                                     "' points to non-existent node '" + externalArc.getNodeName() + "'" +
                                     " in process definition '" + externalArc.getExternal() + "'" );
        }

        String arcName = SvUtil.isBlankOrNull( externalArc.getName() ) ? Arc.DEFAULT_ARC : externalArc.getName();

        if ( externalArc.getType() == XmlExternalArcType.OUT )
        {
          engine.getFactory().createArc( graph, localNode, extNode, arcName );
        }
        else
        {
          engine.getFactory().createArc( graph, extNode, localNode, arcName );
        }
      }
    }
  }

  protected Map<String,Node> importInstance (String externalName, String instanceName)
      throws ImportException
  {
    Map<String, Node> nodeMap = new HashMap<String, Node>();
    Graph instanceGraph = engine.getRepository().getLatestGraph( externalName );

    if ( instanceGraph == null )
    {
      throw new ImportException( "Referenced external '" + externalName + "' not found in database" );
    }

    Map<Node,Node> lookupMap = new HashMap<Node, Node>();

    for ( Node node : instanceGraph.getNodes() )
    {
      Node newNode = engine.getFactory().importNode( getGraph(), node, instanceName);

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
      engine.getFactory().createArc( getGraph(), startNode, endNode, arc.getName() );
    }

    return nodeMap;
  }

  @SuppressWarnings("unchecked")
  @Override
  public void importDefinition (XmlWorkflow xmlDef) throws ImportException
  {
    instanceCache = new HashMap<String, Map<String,Node>>();
    nodeCache     = new HashMap<String, Node>();

    Graph latest = engine.getRepository().getLatestGraph( xmlDef.getName() );

    int version = latest == null ? 1 : latest.getVersion() + 1;

    graph = (G)engine.getFactory().newGraph( xmlDef.getName(), version );
    importNodes( xmlDef );
    importArcs( xmlDef );
    importExternalArcs(  xmlDef );
  }

  public void importWithDependencies (String name, XmlWorkflowResolver resolver)
    throws JAXBException, ImportException
  {
    importWithDependencies( name, resolver, new ArrayList<String>( 10 ) );
  }

  private void importWithDependencies (String name, XmlWorkflowResolver resolver, ArrayList<String> stack)
      throws JAXBException, ImportException
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
          throw new ImportException( "Process definition '" + name + "' contains an illegal recursive reference to '" + extName + "'" );
        }

        if ( !isLoaded( extName ) )
        {
          importWithDependencies( extName, resolver, stack );
        }
      }
    }

    stack.remove( stack.size() - 1 );

    importDefinition( xmlDef );
  }

  @Override
  public boolean isLoaded(String name)
  {
    return null != engine.getRepository().getLatestGraph( name );
  }
}