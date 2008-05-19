/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.codemonk.wf.IArc;

@Entity
@Table (name="wf_arc")
public class Arc implements IArc
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long id;
  protected String name;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="graph_id")
  protected Graph graph;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="a_node_ref_id")
  protected NodeRef startNode;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="z_node_ref_id")
  protected NodeRef endNode;

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

  public Graph getGraph ()
  {
    return graph;
  }

  public void setGraph (Graph graph)
  {
    this.graph = graph;
  }

  @Override
  public NodeRef getStartNode ()
  {
    return startNode;
  }

  public void setStartNode (NodeRef startNode)
  {
    this.startNode = startNode;
  }

  @Override
  public NodeRef getEndNode ()
  {
    return endNode;
  }

  public void setEndNode (NodeRef endNode)
  {
    this.endNode = endNode;
  }

  @Override
  public String toString ()
  {
    return "[Arc id=" + id + " name=" + name + "]";
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
    if ( !( obj instanceof Arc ) ) return false;
    final Arc other = (Arc)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}