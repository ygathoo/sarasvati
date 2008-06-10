package org.codemonk.wf;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBException;

import org.codemonk.wf.xml.XmlArc;
import org.codemonk.wf.xml.XmlExternalArc;
import org.codemonk.wf.xml.XmlExternalArcType;
import org.codemonk.wf.xml.XmlNode;
import org.codemonk.wf.xml.XmlWorkflow;
import org.codemonk.wf.xml.XmlWorkflowResolver;

public abstract class BaseWfLoader<G extends WfGraph,N extends Node> implements WfLoader<G,N>
{
  protected Map<String,Map<String,N>> instanceCache = null;
  protected Map<String,N>             nodeCache     = null;

  protected G graph;

  protected G getWfGraph ()
  {
    return graph;
  }

  protected abstract G createWfGraph (String name);

  protected abstract void createArc (N startNode, N endNode, String name) throws ImportException;

  protected abstract N createNode (String name,
                                   String type,
                                   boolean isJoin,
                                   boolean isStart,
                                   String guard,
                                   Object custom)
    throws ImportException;

  protected abstract Map<String,N> importInstance (String externalName, String instanceName) throws ImportException;

  protected void importNodes (XmlWorkflow xmlDef) throws ImportException
  {
    for ( XmlNode xmlNode : xmlDef.getNodes() )
    {
      String nodeName = xmlNode.getName();

      if ( nodeCache.containsKey( nodeName ) )
      {
        throw new ImportException( "Node name '" + nodeName + "' is not unique in workflow: " + getWfGraph().getName() );
      }

      String type = xmlNode.getType();

      N newNode = createNode( nodeName,
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
        N startNode = nodeCache.get( xmlNode.getName() );
        N endNode   = nodeCache.get( xmlArc.getTo() );

        if ( endNode == null )
        {
          throw new ImportException( "Arc in node '" + xmlNode.getName() + "' points to non-existent node '" + xmlArc.getTo() + "'" );
        }

        createArc( startNode, endNode, xmlArc.getName() == null ? "" : xmlArc.getName() );
      }
    }
  }

  protected String getInstanceKey (XmlExternalArc externalArc)
  {
    return externalArc.getExternal() + ":" + externalArc.getInstance();
  }

  protected N getExternalNode (XmlExternalArc externalArc) throws ImportException
  {
    Map<String,N> instance = instanceCache.get( getInstanceKey( externalArc ) );

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
        N localNode = nodeCache.get( xmlNode.getName() );
        N extNode = getExternalNode( externalArc );

        if ( extNode == null )
        {
          throw new ImportException( "External arc in node '" + xmlNode.getName() +
                                     "' points to non-existent node '" + externalArc.getNodeName() + "'" +
                                     " in process definition '" + externalArc.getExternal() + "'" );
        }

        if ( externalArc.getType() == XmlExternalArcType.OUT )
        {
          createArc( localNode, extNode, externalArc.getName() );
        }
        else
        {
          createArc( extNode, localNode, externalArc.getName() );
        }
      }
    }
  }

  @Override
  public void importDefinition (XmlWorkflow xmlDef) throws ImportException
  {
    instanceCache = new HashMap<String, Map<String,N>>();
    nodeCache     = new HashMap<String, N>();

    graph = createWfGraph( xmlDef.getName() );
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
}