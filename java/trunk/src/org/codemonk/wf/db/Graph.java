/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.codemonk.wf.IArc;
import org.codemonk.wf.IGraph;
import org.codemonk.wf.INode;

@Entity
@Table (name="wf_graph")
public class Graph implements IGraph
{
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  protected Long   id;
  protected String name;
  protected int    version;

  @OneToMany (fetch=FetchType.EAGER, mappedBy="graph")
  protected List<NodeRef> nodeRefs;

  @OneToMany (fetch=FetchType.EAGER, mappedBy="graph")
  protected List<Arc>     arcs;

  @Transient
  protected Map<NodeRef, List<IArc>> inputMap;

  @Transient
  protected Map<NodeRef, List<IArc>> outputMap;

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public int getVersion ()
  {
    return version;
  }

  public void setVersion (int version)
  {
    this.version = version;
  }

  public List<NodeRef> getNodeRefs ()
  {
    return nodeRefs;
  }

  public void setNodeRefs (List<NodeRef> nodeRefs)
  {
    this.nodeRefs = nodeRefs;
  }

  public List<Arc> getArcs ()
  {
    return arcs;
  }

  public void setArcs (List<Arc> arcs)
  {
    this.arcs = arcs;
  }

  @Override
  public List<IArc> getInputArcs (INode node)
  {
    return inputMap.get( node );
  }

  @Override
  public List<IArc> getInputArcs (INode node, String arcName)
  {
    List<IArc> arcList = getInputArcs( node );
    List<IArc> result = new ArrayList<IArc>( arcList.size() );

    for ( IArc arc : arcList )
    {
      if ( arcName.equals( arc.getName() ) )
      {
        result.add( arc );
      }
    }
    return result;
  }

  @Override
  public List<IArc> getOutputArcs (INode node)
  {
    return outputMap.get( node );
  }

  @Override
  public List<IArc> getOutputArcs (INode node, String arcName)
  {
    List<IArc> arcList = getOutputArcs( node );
    List<IArc> result = new ArrayList<IArc>( arcList.size() );

    for ( IArc arc : arcList )
    {
      if ( arcName.equals( arc.getName() ) )
      {
        result.add( arc );
      }
    }
    return result;
  }

  public void ensureInitialized ()
  {
    if ( inputMap != null && outputMap != null )
    {
      return;
    }

    inputMap  = new HashMap<NodeRef, List<IArc>>();
    outputMap = new HashMap<NodeRef, List<IArc>>();

    for ( Arc arc : arcs )
    {
      NodeRef node = arc.getStartNode();
      List<IArc> list = outputMap.get( node );

      if ( list == null )
      {
        list = new LinkedList<IArc>();
        outputMap.put( node, list );
      }

      list.add( arc );

      node = arc.getEndNode();
      list = inputMap.get( node );

      if ( list == null )
      {
        list = new LinkedList<IArc>();
        inputMap.put( node, list );
      }

      list.add( arc );
    }

    List<IArc> emptyList = Collections.emptyList();
    for (NodeRef node : nodeRefs )
    {
      if ( !inputMap.containsKey( node ) )
      {
        inputMap.put( node, emptyList );
      }
      if ( !outputMap.containsKey( node ) )
      {
        outputMap.put( node, emptyList );
      }
    }
  }

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( id == null )
        ? 0 : id.hashCode() );
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if ( this == obj ) return true;
    if ( obj == null ) return false;
    if ( !( obj instanceof Graph ) ) return false;
    final Graph other = (Graph)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}
