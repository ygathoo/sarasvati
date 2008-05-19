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
import org.codemonk.wf.GuardAction;
import org.codemonk.wf.IArc;
import org.codemonk.wf.INode;
import org.codemonk.wf.INodeToken;
import org.codemonk.wf.IProcess;
import org.hibernate.annotations.Type;

@Entity
@Table (name="wf_node")
@Inheritance (strategy=InheritanceType.JOINED)
public class Node implements INode
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long   id;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn( name="graph_id")
  protected Graph graph;

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

  public Graph getGraph ()
  {
    return graph;
  }

  public void setGraph (Graph graph)
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
  public GuardAction guard (IProcess process, INodeToken token)
  {
    return GuardAction.AcceptToken;
  }

  @Override
  public void execute (Engine engine, IProcess process, INodeToken token)
  {
    engine.completeExecuteNode( process, token, IArc.DEFAULT_ARC );
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
    if ( !( obj instanceof Node ) ) return false;
    final Node other = (Node)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}