/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.codemonk.wf.INodeToken;

@Entity
@Table(name="wf_node_token")
public class NodeToken implements INodeToken
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long    id;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "process_id")
  protected Process process;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "node_ref_id")
  protected NodeRef nodeRef;

  @ManyToMany (fetch=FetchType.LAZY, cascade= {CascadeType.ALL})
  @JoinTable( name = "wf_node_token_parent",
              joinColumns = @JoinColumn(name = "node_token_id"),
              inverseJoinColumns = @JoinColumn(name = "arc_token_id") )
  protected List<ArcToken> parentTokens;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="create_date", updatable = false)
  protected Date    createDate;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="complete_date")
  protected Date    completeDate;

  public NodeToken () { /* Default constructor for Hibernate */ }

  public NodeToken (Process process, NodeRef nodeRef, List<ArcToken> parentTokens)
  {
    this.process      = process;
    this.nodeRef      = nodeRef;
    this.parentTokens = parentTokens;
    this.createDate   = new Date();
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  public Process getProcess ()
  {
    return process;
  }

  public void setProcess (Process process)
  {
    this.process = process;
  }

  @Override
  public NodeRef getNode ()
  {
    return nodeRef;
  }

  public void setNodeRef (NodeRef nodeRef)
  {
    this.nodeRef = nodeRef;
  }

  public List<ArcToken> getParentTokens ()
  {
    return parentTokens;
  }

  public void setParentTokens (List<ArcToken> parentTokens)
  {
    this.parentTokens = parentTokens;
  }

  public Date getCreateDate ()
  {
    return createDate;
  }

  public void setCreateDate (Date createDate)
  {
    this.createDate = createDate;
  }

  public Date getCompleteDate ()
  {
    return completeDate;
  }

  public void setCompleteDate (Date completeDate)
  {
    this.completeDate = completeDate;
  }

  @Override
  public void markComplete ()
  {
    this.completeDate = new Date();
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
    if ( !( obj instanceof NodeToken ) ) return false;
    final NodeToken other = (NodeToken)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}