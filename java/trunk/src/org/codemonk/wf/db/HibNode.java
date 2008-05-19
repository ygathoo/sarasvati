/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Inheritance;
import javax.persistence.InheritanceType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.codemonk.wf.Engine;
import org.codemonk.wf.Arc;
import org.codemonk.wf.GuardResponse;
import org.codemonk.wf.Node;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.Process;
import org.hibernate.annotations.Type;

@Entity
@Table (name="wf_node")
@Inheritance (strategy=InheritanceType.JOINED)
public class HibNode implements Node
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long   id;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn( name="graph_id")
  protected HibGraph graph;

  protected String name;
  protected String type;

  @Column (name="is_join")
  @Type (type="yes_no")
  protected boolean join;

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

  @Override
  public String getType ()
  {
    return type;
  }

  public void setType (String type)
  {
    this.type = type;
  }

  public HibGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (HibGraph graph)
  {
    this.graph = graph;
  }

  @Override
  public boolean isJoin ()
  {
    return join;
  }

  public void setJoin (boolean join)
  {
    this.join = join;
  }

  @Override
  public String getLabel ()
  {
    return "";
  }

  @Override
  public GuardResponse guard (Process process, NodeToken token)
  {
    return GuardResponse.ACCEPT_TOKEN_RESPONSE;
  }

  @Override
  public void execute (Engine engine, Process process, NodeToken token)
  {
    engine.completeExecuteNode( process, token, Arc.DEFAULT_ARC );
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
    if ( !( obj instanceof HibNode ) ) return false;
    final HibNode other = (HibNode)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}