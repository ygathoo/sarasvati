/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.codemonk.wf.IArcToken;

@Entity
@Table (name="wf_arc_token")
public class ArcToken implements IArcToken
{
  @Id
  @GeneratedValue (strategy=GenerationType.IDENTITY)
  protected Long    id;

  @ManyToOne (fetch = FetchType.EAGER)
  @JoinColumn (name = "process_id")
  protected Process process;

  @ManyToOne (fetch = FetchType.EAGER)
  @JoinColumn (name = "arc_id")
  protected Arc     arc;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn (name = "parent_token_id", nullable=false)
  protected NodeToken parentToken;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="create_date", updatable = false)
  protected Date    createDate;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="complete_date")
  protected Date    completeDate;

  public ArcToken () { /* Default constructor for hibernate */ }

  public ArcToken (Process process, Arc arc, NodeToken parentToken)
  {
    this.process     = process;
    this.arc         = arc;
    this.parentToken = parentToken;
    this.createDate  = new Date();
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
  public Arc getArc ()
  {
    return arc;
  }

  public void setArc (Arc arc)
  {
    this.arc = arc;
  }

  public NodeToken getParentToken ()
  {
    return parentToken;
  }

  public void setPreviousToken (NodeToken parentToken)
  {
    this.parentToken = parentToken;
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
    if ( !( obj instanceof ArcToken ) ) return false;
    final ArcToken other = (ArcToken)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}