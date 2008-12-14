/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
/**
 * Created on Apr 25, 2008
 */
package com.googlecode.sarasvati.hib;

import java.util.Date;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import org.hibernate.annotations.Type;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeToken;

@Entity
@Table (name="wf_arc_token")
public class HibArcToken implements ArcToken
{
  @Id
  @GeneratedValue (strategy=GenerationType.IDENTITY)
  protected Long    id;

  @ManyToOne (fetch = FetchType.EAGER)
  @JoinColumn (name = "process_id")
  protected HibGraphProcess process;

  @ManyToOne (fetch = FetchType.EAGER)
  @JoinColumn (name = "arc_id")
  protected HibArc     arc;

  @ManyToOne(fetch = FetchType.LAZY, targetEntity=HibNodeToken.class)
  @JoinColumn (name = "parent_token_id", nullable=false)
  protected NodeToken parentToken;

  @ManyToOne( fetch=FetchType.LAZY, targetEntity=HibNodeToken.class, cascade=CascadeType.ALL)
  @JoinTable( name = "wf_node_token_parent",
              joinColumns = @JoinColumn(name = "arc_token_id"),
              inverseJoinColumns = @JoinColumn(name = "node_token_id") )
  protected NodeToken childToken;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="create_date", updatable = false)
  protected Date    createDate;

  @Temporal(TemporalType.TIMESTAMP)
  @Column (name="complete_date")
  protected Date    completeDate;

  @Type (type="yes_no")
  protected boolean pending;

  public HibArcToken () { /* Default constructor for hibernate */ }

  public HibArcToken (HibGraphProcess process, HibArc arc, HibNodeToken parentToken)
  {
    this.process     = process;
    this.arc         = arc;
    this.parentToken = parentToken;
    this.createDate  = new Date();
    this.pending     = true;
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  public HibGraphProcess getProcess ()
  {
    return process;
  }

  public void setProcess (HibGraphProcess process)
  {
    this.process = process;
  }

  @Override
  public HibArc getArc ()
  {
    return arc;
  }

  public void setArc (HibArc arc)
  {
    this.arc = arc;
  }

  public NodeToken getParentToken ()
  {
    return parentToken;
  }

  public void setParentToken (NodeToken parentToken)
  {
    this.parentToken = parentToken;
  }

  public NodeToken getChildToken()
  {
    return childToken;
  }

  public void setChildToken(NodeToken childToken)
  {
    this.childToken = childToken;
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
  public boolean isComplete ()
  {
    return completeDate != null;
  }

  @Override
  public void markComplete (Engine engine, NodeToken token)
  {
    this.completeDate = new Date();
    this.childToken = token;
  }

  @Override
  public boolean isPending ()
  {
    return pending;
  }

  @Override
  public void markProcessed (Engine engine)
  {
    pending = false;
  }

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( id == null ) ? 0 : id.hashCode() );
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if ( this == obj ) return true;
    if ( obj == null ) return false;
    if ( !( obj instanceof HibArcToken ) ) return false;
    final HibArcToken other = (HibArcToken)obj;
    if ( id == null )
    {
      if ( other.getId() != null ) return false;
    }
    else if ( !id.equals( other.getId() ) ) return false;
    return true;
  }
}