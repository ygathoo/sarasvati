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
import org.codemonk.wf.INode;
import org.codemonk.wf.INodeToken;
import org.codemonk.wf.IProcess;

@Entity
@Table (name="wf_node_ref")
public class NodeRef implements INode
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long   id;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn(name="node_id")
  protected Node node;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="graph_id")
  protected Graph graph;

  protected String instance;

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  public Node getNode ()
  {
    return node;
  }

  public void setNode (Node node)
  {
    this.node = node;
  }

  public Graph getGraph ()
  {
    return graph;
  }

  public void setGraph (Graph graph)
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
  public GuardResponse guard (IProcess process, INodeToken token)
  {
    return node.guard( process, token );
  }

  @Override
  public void execute (Engine engine, IProcess process, INodeToken token)
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
    if ( !( obj instanceof NodeRef ) ) return false;
    final NodeRef other = (NodeRef)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}