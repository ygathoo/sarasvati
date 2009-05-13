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

import java.io.File;
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
import com.googlecode.sarasvati.xml.XmlExternal;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;
import com.googlecode.sarasvati.xml.XmlProcessDefinitionResolver;

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

  protected void importNodes (XmlProcessDefinition xmlDef)
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

      List<Object> customData = xmlNode.getCustom() == null ? null : xmlNode.getCustom().getCustom();

      Node newNode = factory.newNode( graph, nodeName,
                                      type == null ? "node" : type,
                                      xmlNode.getJoinType().getJoinType(),
                                      xmlNode.isStart(),
                                      xmlNode.getGuard(),
                                      customData );
      nodeCache.put( nodeName, newNode );
    }
  }

  protected void importArcs (XmlProcessDefinition xmlDef) throws LoadException
  {
    for (XmlNode xmlNode : xmlDef.getNodes())
    {
      for ( XmlArc xmlArc : xmlNode.getArcs() )
      {
        Node startNode = nodeCache.get( xmlNode.getName() );
        Node endNode = null;

        if ( !SvUtil.isBlankOrNull( xmlArc.getExternal() ) )
        {
          endNode = getExternalNode( xmlArc.getExternal(), xmlArc.getTo() );
          if ( endNode == null )
          {
            throw new LoadException( "Arc in node '" + xmlNode.getName() +
                                     "' points to non-existent node '" + xmlArc.getTo() +
                                     "' in external '" + xmlArc.getExternal() + "'" );
          }
        }
        else
        {
          endNode   = nodeCache.get( xmlArc.getTo() );
          if ( endNode == null )
          {
            throw new LoadException( "Arc in node '" + xmlNode.getName() + "' points to non-existent node '" + xmlArc.getTo() + "'" );
          }
        }

        factory.newArc( graph, startNode, endNode, SvUtil.isBlankOrNull( xmlArc.getName() ) ? Arc.DEFAULT_ARC : xmlArc.getName() );
      }
    }
  }

  protected void importExternals (XmlProcessDefinition xmlDef) throws LoadException
  {
    for ( XmlExternal external : xmlDef.getExternals() )
    {
      Map<String,Node> instance = importInstance( external.getProcessDefinition(), external.getName() );
      instanceCache.put( external.getName(), instance );
    }
  }

  protected Node getExternalNode (String external, String node) throws LoadException
  {
    Map<String,Node> instance = instanceCache.get( external );

    if (instance == null)
    {
      throw new LoadException( "Referenced external '" + external + "' not defined." );
    }

    return instance.get( node );
  }

  protected void importExternalArcs (XmlProcessDefinition xmlDef) throws LoadException
  {
    for ( XmlExternal xmlExternal : xmlDef.getExternals() )
    {
      for ( XmlExternalArc xmlExternalArc : xmlExternal.getExternalArcs() )
      {
        Node startNode = getExternalNode( xmlExternal.getName(), xmlExternalArc.getFrom() );
        Node endNode = null;

        if ( !SvUtil.isBlankOrNull( xmlExternalArc.getExternal() ) )
        {
          endNode = getExternalNode( xmlExternalArc.getExternal(), xmlExternalArc.getTo() );
          if ( endNode == null )
          {
            throw new LoadException( "Arc in external '" + xmlExternal.getName() +
                                     "' points to non-existent node '" + xmlExternalArc.getTo() +
                                     "' in external '" + xmlExternalArc.getExternal() + "'" );
          }
        }
        else
        {
          endNode   = nodeCache.get( xmlExternalArc.getTo() );
          if ( endNode == null )
          {
            throw new LoadException( "Arc in external'" + xmlExternalArc.getName() +
                                     "' points to non-existent node '" + xmlExternalArc.getTo() + "'" );
          }
        }

        String arcName = SvUtil.isBlankOrNull( xmlExternalArc.getName() ) ? Arc.DEFAULT_ARC : xmlExternalArc.getName();

        factory.newArc( graph, startNode, endNode, arcName );
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

  public void loadDefinition (XmlProcessDefinition xmlDef)
    throws LoadException
  {
    loadDefinition( xmlDef, null );
  }

  public void loadDefinition (XmlProcessDefinition xmlDef, GraphValidator validator)
    throws LoadException
  {
    validateXml( xmlDef, validator );

    instanceCache = new HashMap<String, Map<String,Node>>();
    nodeCache     = new HashMap<String, Node>();

    Graph latest = repository.getLatestGraph( xmlDef.getName() );

    int version = latest == null ? 1 : latest.getVersion() + 1;

    graph = factory.newGraph( xmlDef.getName(), version );

    importExternals( xmlDef );
    importNodes( xmlDef );
    importArcs( xmlDef );
    importExternalArcs( xmlDef );

    validateGraph( validator );

    repository.addGraph( graph );
  }

  protected void validateXml (XmlProcessDefinition xmlDef, GraphValidator validator)
    throws LoadException
  {
    try
    {
      if ( validator == null )
      {
        return;
      }

      validator.validateXmlProcessDefinition( xmlDef );
      for ( XmlNode xmlNode : xmlDef.getNodes() )
      {
        validator.validateXmlNode( xmlNode );

        for ( XmlArc xmlArc : xmlNode.getArcs() )
        {
          validator.validateXmlArc( xmlArc );
        }
      }

      for ( XmlExternal xmlExternal : xmlDef.getExternals() )
      {
        validator.validateXmlExternal( xmlExternal );

        for ( XmlExternalArc xmlExternalArc : xmlExternal.getExternalArcs() )
        {
          validator.validateXmlExternalArc( xmlExternalArc );
        }
      }
    }
    catch ( RuntimeException re )
    {
      throw new LoadException( "Failure while loading process definition '" + xmlDef + "'", re );
    }
  }

  protected void validateGraph (GraphValidator validator)
    throws LoadException
  {
    if ( validator == null )
    {
      return;
    }

    validator.validateGraph( graph );

    for ( Node node : graph.getNodes() )
    {
      validator.validateNode( node );
    }

    for ( Arc arc : graph.getArcs() )
    {
      validator.validateArc( arc );
    }
  }

  public void loadWithDependencies (String name, XmlProcessDefinitionResolver resolver)
    throws JAXBException, LoadException
  {
    loadWithDependencies( name, resolver, null );
  }

  public void loadWithDependencies (String name, XmlProcessDefinitionResolver resolver, GraphValidator validator)
    throws JAXBException, LoadException
  {
    loadWithDependencies( name, resolver, validator, new ArrayList<String>() );
  }

  private void loadWithDependencies (String name, XmlProcessDefinitionResolver resolver, GraphValidator validator, List<String> stack)
      throws JAXBException, LoadException
  {
    stack.add( name );
    XmlProcessDefinition xmlDef = resolver.resolve( name );

    for ( XmlExternal external : xmlDef.getExternals() )
    {
      String extName = external.getProcessDefinition();
      if ( stack.contains( extName ) )
      {
        throw new LoadException( "Process definition '" + name + "' contains an illegal recursive reference to '" + extName + "'" );
      }

      if ( !isLoaded( extName ) )
      {
        loadWithDependencies( extName, resolver, validator, stack );
      }
    }

    stack.remove( stack.size() - 1 );

    loadDefinition( xmlDef, validator );
  }

  public boolean isLoaded (String name)
  {
    return null != repository.getLatestGraph( name );
  }

  /**
   * Loads the process definitions defined in the specified XML file
   * into the {@link GraphRepository} that this GraphLoader was constructed
   * with.
   *
   * @param fileName The path to the process definition xml file to load
   *
   * @throws LoadException If there is a problem loading the process definition
   * @throws JAXBException If there is an error loading the xml into objects
   */
  public void load (String fileName) throws LoadException, JAXBException
  {
    load( fileName, null );
  }

  /**
   * Loads the process definitions defined in the specified XML file
   * into the {@link GraphRepository} that this GraphLoader was constructed
   * with.
   *
   * @param fileName The path to the process definition xml file to load
   * @param validator The {@link GraphValidator} to use to validate the xml and graph
   *
   * @throws LoadException If there is a problem loading the process definition
   * @throws JAXBException If there is an error loading the xml into objects
   */
  public void load (String fileName, GraphValidator validator) throws LoadException, JAXBException
  {
    load( new File( fileName ), validator );
  }

  /**
   * Loads the process definitions defined in the specified XML file
   * into the {@link GraphRepository} that this GraphLoader was constructed
   * with.
   *
   * @param file The file to load the the process definition xml from
   *
   * @throws LoadException If there is a problem loading the process definition
   * @throws JAXBException If there is an error loading the xml into objects
   */
  public void load (File file) throws LoadException, JAXBException
  {
    load( file, null );
  }

  /**
   * Loads the process definitions defined in the specified XML file
   * into the {@link GraphRepository} that this GraphLoader was constructed
   * with.
   *
   * @param file The file to load the the process definition xml from
   * @param validator The {@link GraphValidator} to use to validate the xml and graph
   *
   * @throws LoadException If there is a problem loading the process definition
   * @throws JAXBException If there is an error loading the xml into objects
   */
  public void load (File file, GraphValidator validator) throws LoadException, JAXBException
  {
    XmlLoader xmlLoader = new XmlLoader();
    XmlProcessDefinition xmlDef = xmlLoader.loadProcessDefinition( file );
    loadDefinition( xmlDef, validator );
  }
}