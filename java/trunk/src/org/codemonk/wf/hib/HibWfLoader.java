package org.codemonk.wf.hib;

import java.util.HashMap;
import java.util.Map;

import org.codemonk.wf.BaseWfLoader;
import org.codemonk.wf.ImportException;
import org.codemonk.wf.WfGraph;

public class HibWfLoader extends BaseWfLoader<HibWfGraph, HibNodeRef>
{
  public static interface NodeFactory
  {
    HibNode createNode( HibWfEngine engine, HibNode node, Object custom)
      throws ImportException;
  }

  protected Map<String,NodeFactory> customTypeFactories = new HashMap<String, NodeFactory>();
  protected HibWfEngine engine;

  public HibWfLoader (HibWfEngine engine)
  {
    this.engine = engine;
  }

  public void addCustomType (String type, NodeFactory factory)
  {
    customTypeFactories.put( type, factory );
  }

  @Override
  protected HibWfGraph createWfGraph (String name)
  {
    WfGraph latest = engine.getLatestGraph( name );

    int version = latest == null ? 1 : latest.getVersion() + 1;

    HibWfGraph graph = new HibWfGraph( name, version );
    engine.getSession().save( graph );
    return graph;
  }

  @Override
  protected HibNodeRef createNode (String name,
                                   String type,
                                   boolean isJoin,
                                   boolean isStart,
                                   String guard,
                                   Object custom)
    throws ImportException
  {
    HibNode node = new HibNode(getWfGraph(), name, type, isJoin, isStart, guard);

    NodeFactory factory = customTypeFactories.get( type );

    if ( factory == null )
    {
      engine.getSession().save( node );
    }
    else
    {
      node = factory.createNode( engine, node, custom );
    }

    HibNodeRef nodeRef = new HibNodeRef( getWfGraph(), node, "" );
    engine.getSession().save( nodeRef  );
    getWfGraph().getNodeRefs().add( nodeRef );

    return nodeRef;
  }

  @Override
  protected void createArc (HibNodeRef startNode, HibNodeRef endNode, String name)
      throws ImportException
  {
    HibArc arc = new HibArc( getWfGraph(), startNode, endNode, name );
    engine.getSession().save( arc );
    getWfGraph().getArcs().add( arc );
  }

  @Override
  protected Map<String,HibNodeRef> importInstance (String externalName, String instanceName)
      throws ImportException
  {
    HibWfGraph graph = engine.getLatestGraph( externalName );

    if ( graph == null )
    {
      throw new ImportException( "Referenced external '" + externalName + "' not found in database" );
    }

    Map<String, HibNodeRef> refMap = new HashMap<String, HibNodeRef>();
    Map<Long,HibNodeRef>    arcRefMap = new HashMap<Long, HibNodeRef>();

    for ( HibNodeRef nodeRef : graph.getNodeRefs() )
    {
      String label = nodeRef.getInstance();
      label = label == null || "".equals( label ) ? instanceName : instanceName + ":" + label;

      HibNodeRef newRef = new HibNodeRef( getWfGraph(), nodeRef.getNode(), label );
      engine.getSession().save( newRef );

      arcRefMap.put( nodeRef.getId(), newRef );
      if ( nodeRef.isNodeDefinedInTopLevel() )
      {
        refMap.put( nodeRef.getName(), newRef );
      }
    }

    for ( HibArc arc : graph.getArcs() )
    {
      HibNodeRef startNode = arcRefMap.get( arc.getStartNode().getId() );
      HibNodeRef endNode = arcRefMap.get( arc.getEndNode().getId() );
      HibArc newArc = new HibArc( getWfGraph(), startNode, endNode, arc.getName() );
      engine.getSession().save( newArc );
    }

    return refMap;
  }

  public boolean isLoaded (String name)
  {
    return null != engine.getLatestGraph( name );
  }
}