package org.codemonk.wf.visual;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.codemonk.wf.IArc;
import org.codemonk.wf.INode;
import org.codemonk.wf.db.HibGraph;
import org.codemonk.wf.db.HibNodeRef;

public class GraphTree
{
  protected Map<HibNodeRef, GraphTreeNode> nodeMap = new HashMap<HibNodeRef, GraphTreeNode>();

  protected GraphTreeNode root = new GraphTreeNode( null, null );

  protected List<List<GraphTreeNode>> layers = new LinkedList<List<GraphTreeNode>>();

  public GraphTree (HibGraph graph)
  {
    List<GraphTreeNode> nextLayer = new LinkedList<GraphTreeNode>();

    List<INode> startNodes = graph.getStartNodes();

    if ( startNodes.isEmpty() )
    {
      List<HibNodeRef> nodeRefs = graph.getNodeRefs();

      if ( !nodeRefs.isEmpty() )
      {
        startNodes = new ArrayList<INode>(1);
        startNodes.add( nodeRefs.get( 0 ) );
      }
    }

    for ( INode node : startNodes )
    {
      HibNodeRef nodeRef = (HibNodeRef)node;
      GraphTreeNode treeNode = GraphTreeNode.newInstance( root, nodeRef );

      nodeMap.put( nodeRef, treeNode );
      treeNode.addToLayer( nextLayer );
    }


    List<GraphTreeNode> layer = null;

    while ( !nextLayer.isEmpty() )
    {
      layers.add( nextLayer );
      layer = nextLayer;
      nextLayer = new LinkedList<GraphTreeNode>();

      for ( GraphTreeNode treeNode : layer )
      {
        List<IArc> arcs = graph.getOutputArcs( treeNode.getNode() );

        for ( IArc arc : arcs )
        {
          HibNodeRef target = (HibNodeRef)arc.getEndNode();
          GraphTreeNode targetTreeNode = nodeMap.get( target );

          if (targetTreeNode == null)
          {
            targetTreeNode = GraphTreeNode.newInstance( treeNode, target );
            nodeMap.put( target, targetTreeNode );
            targetTreeNode.addToLayer( nextLayer );
          }
        }
      }
    }
  }

  public GraphTreeNode getTreeNode (HibNodeRef node)
  {
    return nodeMap.get( node );
  }

  public int getLayerCount ()
  {
    return layers.size();
  }

  public List<GraphTreeNode> getLayer (int index)
  {
    return layers.get( index );
  }

  public List<List<GraphTreeNode>> getLayers ()
  {
    return layers;
  }
}