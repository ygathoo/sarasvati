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
                        Vincent Kirsch
*/

package com.googlecode.sarasvati.load;

import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.External;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.load.definition.ArcDefinition;
import com.googlecode.sarasvati.load.definition.ExternalArcDefinition;
import com.googlecode.sarasvati.load.definition.ExternalDefinition;
import com.googlecode.sarasvati.load.definition.NodeDefinition;
import com.googlecode.sarasvati.load.definition.ProcessDefinition;
import com.googlecode.sarasvati.util.FileVisitor;
import com.googlecode.sarasvati.util.SvUtil;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

/**
 * Given a {@link GraphFactory} to construct the {@link Graph} parts,
 * a {@link GraphRepository} and a {@link ProcessDefinitionTranslator} to translate the
 * "raw" definition (for example an XML file) into a ProcessDefinition it can understand,
 * the GraphLoader will load process definitions into that repository.
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

  protected void importNodes (final ProcessDefinition procDef) throws SarasvatiLoadException
  {
    for ( NodeDefinition nodeDef : procDef.getNodes() )
    {
      String nodeName = nodeDef.getName();

      if ( nodeCache.containsKey( nodeName ) )
      {
        throw new SarasvatiLoadException( "Node name '" + nodeName + "' is not unique in workflow: " + graph.getName() );
      }

      String type = nodeDef.getType();

      List<Object> customData = nodeDef.getCustom() == null ? null : nodeDef.getCustom().getCustom();

      Node newNode = factory.newNode( graph, nodeName,
                                      type == null ? "node" : type,
                                      nodeDef.getJoinType(),
                                      nodeDef.getJoinParam(),
                                      nodeDef.isStart(),
                                      nodeDef.getGuard(),
                                      customData );
      nodeCache.put( nodeName, newNode );
    }
  }

  protected void importArcs (final ProcessDefinition procDef) throws SarasvatiLoadException
  {
    for ( NodeDefinition nodeDef : procDef.getNodes() )
    {
      for ( ArcDefinition arcDef : nodeDef.getArcs() )
      {
        Node startNode = nodeCache.get( nodeDef.getName() );
        Node endNode = null;

        if ( !SvUtil.isBlankOrNull( arcDef.getExternal() ) )
        {
          endNode = getExternalNode( arcDef.getExternal(), arcDef.getTo() );
          if ( endNode == null )
          {
            throw new SarasvatiLoadException( "Arc in node '" + nodeDef.getName() +
                                     "' points to non-existent node '" + arcDef.getTo() +
                                     "' in external '" + arcDef.getExternal() + "'" );
          }
        }
        else
        {
          endNode = nodeCache.get( arcDef.getTo() );
          if ( endNode == null )
          {
            throw new SarasvatiLoadException( "Arc in node '" + nodeDef.getName() + "' points to non-existent node '" + arcDef.getTo() + "'" );
          }
        }

        factory.newArc( graph, startNode, endNode, SvUtil.isBlankOrNull( arcDef.getName() ) ? Arc.DEFAULT_ARC : arcDef.getName() );
      }
    }
  }

  protected void importExternals (final ProcessDefinition procDef)
  {
    for ( ExternalDefinition externalDefinition : procDef.getExternals() )
    {
      Map<String,Node> instance = importInstance( externalDefinition );
      instanceCache.put( externalDefinition.getName(), instance );
    }
  }

  protected Node getExternalNode (final String external, final String node)
  {
    Map<String,Node> instance = instanceCache.get( external );

    if ( instance == null )
    {
      throw new SarasvatiLoadException( "Referenced external '" + external + "' not defined." );
    }

    return instance.get( node );
  }

  protected void importExternalArcs (final ProcessDefinition procDef)
  {
    for ( ExternalDefinition externalDef : procDef.getExternals() )
    {
      for ( ExternalArcDefinition externalArcDef : externalDef.getExternalArcs() )
      {
        Node startNode = getExternalNode( externalDef.getName(), externalArcDef.getFrom() );
        Node endNode = null;

        if ( !SvUtil.isBlankOrNull( externalArcDef.getExternal() ) )
        {
          endNode = getExternalNode( externalArcDef.getExternal(), externalArcDef.getTo() );
          if ( endNode == null )
          {
            throw new SarasvatiLoadException( "Arc in external '" + externalDef.getName() +
                                     "' points to non-existent node '" + externalArcDef.getTo() +
                                     "' in external '" + externalArcDef.getExternal() + "'" );
          }
        }
        else
        {
          endNode = nodeCache.get( externalArcDef.getTo() );
          if ( endNode == null )
          {
            throw new SarasvatiLoadException( "Arc in external'" + externalArcDef.getName() +
                                     "' points to non-existent node '" + externalArcDef.getTo() + "'" );
          }
        }

        String arcName = SvUtil.isBlankOrNull( externalArcDef.getName() ) ? Arc.DEFAULT_ARC : externalArcDef.getName();

        factory.newArc( graph, startNode, endNode, arcName );
      }
    }
  }

  protected Map<String,Node> importInstance (final ExternalDefinition externalDefinition)
  {
    Map<String, Node> nodeMap = new HashMap<String, Node>();
    Graph instanceGraph = repository.getLatestGraph( externalDefinition.getProcessDefinition() );

    if ( instanceGraph == null )
    {
      throw new SarasvatiLoadException( "Referenced external '" + externalDefinition.getProcessDefinition() + "' not found in database" );
    }

    External external = factory.newExternal( externalDefinition.getName(),
                                             graph,
                                             instanceGraph,
                                             externalDefinition.getCustom() );

    Map<Node,Node> lookupMap = new HashMap<Node, Node>();

    for ( Node node : instanceGraph.getNodes() )
    {
      Node newNode = factory.importNode( graph, node, external );

      lookupMap.put( node, newNode );
      if ( !node.isImportedFromExternal() )
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

  public <T> void loadDefinition (final ProcessDefinitionTranslator<T> translator,
                                  final T source)
    throws SarasvatiLoadException
  {
    loadDefinition( translator, source, null );
  }

  public <T> void loadDefinition (final ProcessDefinitionTranslator<T> translator,
                                  final String customId,
                                  final T source)
    throws SarasvatiLoadException
  {
    loadDefinition( translator, source, customId, null );
  }

  public <T> void loadDefinition (final ProcessDefinitionTranslator<T> translator,
                                  final T source,
                                  final GraphValidator validator)
    throws SarasvatiLoadException
  {
    ProcessDefinition def = translator.translate( source );
    loadDefinition( def, null, validator );
  }

  public <T> void loadDefinition (final ProcessDefinitionTranslator<T> translator,
                                  final T source,
                                  final String customId,
                                  final GraphValidator validator)
    throws SarasvatiLoadException
  {
    ProcessDefinition def = translator.translate( source );
    loadDefinition( def, customId, validator );
  }

  public void loadDefinition (final ProcessDefinition procDef)
    throws SarasvatiLoadException
  {
    loadDefinition( procDef, null, null );
  }

  public void loadDefinition (final ProcessDefinition procDef,
                              final String customId)
    throws SarasvatiLoadException
  {
    loadDefinition( procDef, customId, null );
  }

  public void loadDefinition (final ProcessDefinition procDef,
                              final GraphValidator validator)
    throws SarasvatiLoadException
  {
    loadDefinition( procDef, null, validator );
  }

  public void loadDefinition (final ProcessDefinition procDef,
                              final String customId,
                              final GraphValidator validator)
      throws SarasvatiLoadException
  {
    validateXml( procDef, validator );

    instanceCache = new HashMap<String, Map<String,Node>>();
    nodeCache     = new HashMap<String, Node>();

    Graph latest = repository.getLatestGraph( procDef.getName() );

    int version = latest == null ? 1 : latest.getVersion() + 1;

    graph = factory.newGraph( procDef.getName(), version, customId );

    importExternals( procDef );
    importNodes( procDef );
    importArcs( procDef );
    importExternalArcs( procDef );

    validateGraph( validator );

    repository.addGraph( graph );
  }

  protected void validateXml (final ProcessDefinition procDef,
                              final GraphValidator validator)
      throws SarasvatiLoadException
  {
    try
    {
      if ( validator == null )
      {
        return;
      }

      validator.validateProcessDefinition( procDef );
      for ( NodeDefinition nodeDef : procDef.getNodes() )
      {
        validator.validateNodeDefinition( nodeDef );

        for ( ArcDefinition arcDef : nodeDef.getArcs() )
        {
          validator.validateArcDefinition( arcDef );
        }
      }

      for ( ExternalDefinition externalDef : procDef.getExternals() )
      {
        validator.validateExternalDefinition( externalDef );

        for ( ExternalArcDefinition externalArcDef : externalDef.getExternalArcs() )
        {
          validator.validateExternalArcDefinition( externalArcDef );
        }
      }
    }
    catch ( RuntimeException re )
    {
      throw new SarasvatiLoadException( "Failure while loading process definition '" + procDef + "'", re );
    }
  }

  protected void validateGraph (final GraphValidator validator) throws SarasvatiLoadException
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


  public void loadWithDependencies (final String name,
                                    final ProcessDefinitionResolver resolver)
    throws SarasvatiLoadException
  {
    loadWithDependencies( name, resolver, null );
  }

  public void loadWithDependencies (final String name,
                                    final ProcessDefinitionResolver resolver,
                                    final GraphValidator validator)
    throws SarasvatiLoadException
  {
    loadWithDependencies( name, resolver, validator, new ArrayList<String>() );
  }

  private void loadWithDependencies (final String name,
                                     final ProcessDefinitionResolver resolver,
                                     final GraphValidator validator,
                                     final List<String> stack)
    throws SarasvatiLoadException
  {
    stack.add( name );
    ProcessDefinition procDef = resolver.resolve( name );

    for ( ExternalDefinition external : procDef.getExternals() )
    {
      String extName = external.getProcessDefinition();
      if ( stack.contains( extName ) )
      {
        throw new SarasvatiLoadException( "Process definition '" + name + "' contains an illegal recursive reference to '" + extName + "'" );
      }

      if ( !isLoaded( extName ) )
      {
        loadWithDependencies( extName, resolver, validator, stack );
      }
    }

    stack.remove( stack.size() - 1 );

    loadDefinition( procDef, validator );
  }

  public boolean isLoaded (final String name)
  {
    return null != repository.getLatestGraph( name );
  }

  public List<LoadResult> loadNewAndChanged (final File basePath,
                                             final GraphValidator validator)
    throws SarasvatiLoadException
  {
    FilenameFilter filenameFilter = new FilenameFilter()
    {
      @Override
      public boolean accept (File dir, String name)
      {
        return name.endsWith( ".wf.xml" );
      }
    };

    return loadNewAndChanged( basePath, filenameFilter, validator );
  }

  public List<LoadResult> loadNewAndChanged (final File basePath,
                                             final FilenameFilter filenameFilter,
                                             final GraphValidator validator)
    throws SarasvatiLoadException
  {
    final XmlLoader xmlLoader = new XmlLoader();
    final Map<String, XmlProcessDefinition> processDefs = new HashMap<String, XmlProcessDefinition>();

    FileVisitor visitor = new FileVisitor()
    {
      @Override
      public boolean accept (File dir, String name)
      {
        return filenameFilter.accept( dir, name );
      }

      public void accept (File file)
      {
        XmlProcessDefinition pd = xmlLoader.translate( file );
        processDefs.put( pd.getName(), pd );
      }
    };

    Set<String> updated = new HashSet<String>();

    try
    {
      SvUtil.visitRecursive( basePath, visitor );
    }
    catch (IOException ioe)
    {
      throw new SarasvatiLoadException( "Error loading workflows", ioe );
    }

    List<LoadResult> loadResults = new LinkedList<LoadResult>();

    // Find all process definitions that are new or have changed
    for ( XmlProcessDefinition processDefinition : processDefs.values() )
    {
      String name   = processDefinition.getName();
      String digest = processDefinition.getMessageDigest();

      Graph latest = repository.getLatestGraph( name );
      if ( latest == null )
      {
        updated.add( name );
        loadResults.add( LoadResult.newGraph( name ) );
      }
      else if ( !digest.equals( latest.getCustomId() ) )
      {
        updated.add( name );
        loadResults.add( LoadResult.updatedGraph( name ) );
      }
    }

    // Collection is sorted so that externals are before all places that they are used.
    Collection<XmlProcessDefinition> sorted = SvUtil.getSorted( processDefs );

    // Find all process definition which depend on those that have been updated
    for ( XmlProcessDefinition processDefinition : sorted )
    {
      String name = processDefinition.getName();
      // If we already know it needs to updated, skip checking this one
      if ( updated.contains( name ) )
      {
        continue;
      }

      for ( ExternalDefinition ext : processDefinition.getExternals() )
      {
        if ( updated.contains( ext.getProcessDefinition() ) )
        {
          updated.add( name );
          loadResults.add( LoadResult.updatedGraph( name, ext.getProcessDefinition() ) );
          break;
        }
      }
    }

    // Load all updated/new process definitions in order
    for ( XmlProcessDefinition processDefinition : sorted )
    {
      if ( updated.contains( processDefinition.getName() ) )
      {
        loadDefinition( processDefinition, processDefinition.getMessageDigest(), validator );
      }
    }

    return loadResults;
  }
}