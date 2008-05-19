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

import org.codemonk.wf.Engine;
import org.codemonk.wf.GuardResponse;
import org.codemonk.wf.Node;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.Process;

@Entity
@Table (name="wf_node_ref")
public class HibNodeRef implements Node
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long   id;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn(name="node_id")
  protected HibNode node;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="graph_id")
  protected HibGraph graph;

  protected String instance;

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  public HibNode getNode ()
  {
    return node;
  }

  public void setNode (HibNode node)
  {
    this.node = node;
  }

  public HibGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (HibGraph graph)
  {
    this.graph = graph;
  }

  public String getInstance ()
  {
    return instance;
  }

  public void setInstance (String instance)
  {
    this.instance = instance;
  }

  public String getName ()
  {
    return node.getName();
  }

  public String getType ()
  {
    return node.getType();
  }

  @Override
  public boolean isJoin ()
  {
    return node.isJoin();
  }

  @Override
  public String getLabel ()
  {
    return node.getLabel();
  }

  @Override
  public GuardResponse guard (Process process, NodeToken token)
  {
    return node.guard( process, token );
  }

  @Override
  public void execute (Engine engine, Process process, NodeToken token)
  {
    node.execute( engine, process, token );
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
    if ( !( obj instanceof HibNodeRef ) ) return false;
    final HibNodeRef other = (HibNodeRef)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}