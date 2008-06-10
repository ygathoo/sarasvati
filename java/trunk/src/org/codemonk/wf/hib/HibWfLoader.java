package org.codemonk.wf.hib;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.codemonk.wf.BaseWfLoader;
import org.codemonk.wf.ImportException;
import org.codemonk.wf.WfGraph;

public class HibWfLoader extends BaseWfLoader<HibWfGraph, HibNodeRef>
{
  protected HibWfEngine engine;

  @Override
  protected void createArc (HibNodeRef startNode, HibNodeRef endNode, String name)
      throws ImportException
  {
    HibArc arc = new HibArc( getWfGraph(), startNode, endNode, name );
    engine.getSession().save( arc );
  }

  @Override
  protected HibNodeRef createNode (String name,
                                   String type,
                                   boolean isJoin,
                                   boolean isStart,
                                   String guard,
                                   List<Object> extraData)
    throws ImportException
  {
    return null;
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
  protected Map<String,HibNodeRef> importInstance (String externalName, String instanceName)
      throws ImportException
  {
    HibWfGraph graph = engine.getLatestGraph( externalName );

    Map<String, HibNodeRef> refMap = new HashMap<String, HibNodeRef>();

    for ( HibNodeRef nodeRef : graph.getNodeRefs() )
    {
      HibNodeRef newRef = new HibNodeRef ();
      newRef.setGraph( getWfGraph() );
      newRef.setNode( nodeRef.getNode() );

      String label = newRef.getInstance();
      label = label == null || "".equals( label ) ? instanceName : instanceName + ":" + label;
      newRef.setInstance( label );
      engine.getSession().save( newRef );

      if ( nodeRef.getGraph().equals( getWfGraph() ) )
      {
        refMap.put( nodeRef.getName(), newRef );
      }
    }
    return refMap;
  }
}