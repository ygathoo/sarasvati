package org.codemonk.wf.hib;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codemonk.wf.BaseWfLoader;
import org.codemonk.wf.ImportException;
import org.codemonk.wf.WfGraph;

public class HibWfLoader extends BaseWfLoader<HibWfGraph, HibNodeRef>
{
  public static interface NodeFactory
  {
    HibNode createNode( HibNode node, List<Object> custom);
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
    HibWfGraph graph = new HibWfGraph( name, latest.getVersion() + 1 );
    engine.getSession().save( graph );
    return graph;
  }  
  
  @Override
  protected HibNodeRef createNode (String name, 
                                   String type, 
                                   boolean isJoin,
                                   boolean isStart, 
                                   String guard, 
                                   List<Object> custom)
    throws ImportException
  {
    HibNode node = new HibNode(getWfGraph(), name, type, isJoin, isStart, guard);
    
    NodeFactory factory = customTypeFactories.get( type );
    if ( type != null )
    {
      node = factory.createNode( node, custom );
    }
    
    engine.getSession().save( node );
    
    HibNodeRef nodeRef = new HibNodeRef( getWfGraph(), node, null );
    engine.getSession().save( nodeRef  );
    return nodeRef;
  }

  @Override
  protected void createArc (HibNodeRef startNode, HibNodeRef endNode, String name)
      throws ImportException
  {
    HibArc arc = new HibArc( getWfGraph(), startNode, endNode, name );
    engine.getSession().save( arc );
  }

  @Override
  protected Map<String,HibNodeRef> importInstance (String externalName, String instanceName)
      throws ImportException
  {
    HibWfGraph graph = engine.getLatestGraph( externalName );

    Map<String, HibNodeRef> refMap = new HashMap<String, HibNodeRef>();

    for ( HibNodeRef nodeRef : graph.getNodeRefs() )
    {
      String label = nodeRef.getInstance();
      label = label == null || "".equals( label ) ? instanceName : instanceName + ":" + label;
      
      HibNodeRef newRef = new HibNodeRef( getWfGraph(), nodeRef.getNode(), label );      
      engine.getSession().save( newRef );

      if ( nodeRef.getGraph().equals( getWfGraph() ) )
      {
        refMap.put( nodeRef.getName(), newRef );
      }
    }
    return refMap;
  }
  
  public boolean isLoaded (String name)
  {
    return null != engine.getLatestGraph( name );
  }
}