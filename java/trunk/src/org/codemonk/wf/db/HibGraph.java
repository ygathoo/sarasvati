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

import org.codemonk.wf.Arc;
import org.codemonk.wf.Graph;
import org.codemonk.wf.Node;

@Entity
@Table (name="wf_graph")
public class HibGraph implements Graph
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long   id;
  protected String name;
  protected int    version;

  @OneToMany (fetch=FetchType.EAGER, mappedBy="graph")
  protected List<HibNodeRef> nodeRefs;

  @OneToMany (fetch=FetchType.EAGER, mappedBy="graph")
  protected List<HibArc>     arcs;

  @Transient
  protected Map<HibNodeRef, List<Arc>> inputMap;

  @Transient
  protected Map<HibNodeRef, List<Arc>> outputMap;

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

  public List<HibNodeRef> getNodeRefs ()
  {
    return nodeRefs;
  }

  public void setNodeRefs (List<HibNodeRef> nodeRefs)
  {
    this.nodeRefs = nodeRefs;
  }

  public List<HibArc> getArcs ()
  {
    return arcs;
  }

  public void setArcs (List<HibArc> arcs)
  {
    this.arcs = arcs;
  }

  @Override
  public List<Arc> getInputArcs (Node node)
  {
    if ( inputMap == null )
    {
      initialize();
    }
    return inputMap.get( node );
  }

  @Override
  public List<Arc> getInputArcs (Node node, String arcName)
  {
    List<Arc> arcList = getInputArcs( node );
    List<Arc> result = new ArrayList<Arc>( arcList.size() );

    for ( Arc arc : arcList )
    {
      if ( arcName.equals( arc.getName() ) )
      {
        result.add( arc );
      }
    }
    return result;
  }

  @Override
  public List<Arc> getOutputArcs (Node node)
  {
    if (outputMap == null)
    {
      initialize();
    }
    return outputMap.get( node );
  }

  @Override
  public List<Arc> getOutputArcs (Node node, String arcName)
  {
    List<Arc> arcList = getOutputArcs( node );
    List<Arc> result = new ArrayList<Arc>( arcList.size() );

    for ( Arc arc : arcList )
    {
      if ( arcName.equals( arc.getName() ) )
      {
        result.add( arc );
      }
    }
    return result;
  }

  public void initialize ()
  {
    inputMap  = new HashMap<HibNodeRef, List<Arc>>();
    outputMap = new HashMap<HibNodeRef, List<Arc>>();

    for ( HibArc arc : arcs )
    {
      HibNodeRef node = arc.getStartNode();
      List<Arc> list = outputMap.get( node );

      if ( list == null )
      {
        list = new LinkedList<Arc>();
        outputMap.put( node, list );
      }

      list.add( arc );

      node = arc.getEndNode();
      list = inputMap.get( node );

      if ( list == null )
      {
        list = new LinkedList<Arc>();
        inputMap.put( node, list );
      }

      list.add( arc );
    }

    List<Arc> emptyList = Collections.emptyList();
    for (HibNodeRef node : nodeRefs )
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
  public List<Node> getStartNodes ()
  {
    List<Node> startNodes = new LinkedList<Node>();

    for ( HibNodeRef node : getNodeRefs() )
    {
      if ( "start".equals( node.getType() ) && node.getGraph().equals( this ) )
      {
        startNodes.add( node );
      }
    }

    return startNodes;
  }

  @Override
  public boolean hasArcInverse( Arc arc )
  {
    for (Arc tmpArc : arcs)
    {
      if ( arc.getStartNode().equals( tmpArc.getEndNode() ) &&
           arc.getEndNode().equals( tmpArc.getStartNode() ) )
      {
        return true;
      }
    }

    return false;
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
    if ( !( obj instanceof HibGraph ) ) return false;
    final HibGraph other = (HibGraph)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}
