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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;
import javax.persistence.Transient;

import org.hibernate.annotations.CollectionOfElements;
import org.hibernate.annotations.Where;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.MapEnv;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.Process;
import com.googlecode.sarasvati.ProcessState;

@Entity
@Table (name="wf_process")
public class HibProcess implements Process
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long id;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn( name="graph_id")
  protected HibGraph            graph;

  @OneToMany (mappedBy="process", targetEntity=HibArcToken.class, fetch=FetchType.LAZY)
  @Where (clause="complete_date is null")
  protected List<ArcToken>  arcTokens;

  @OneToMany (mappedBy="process", targetEntity=HibNodeToken.class, fetch=FetchType.LAZY)
  @Where (clause="complete_date is null")
  protected List<NodeToken>  nodeTokens;

  @ManyToOne(fetch = FetchType.LAZY, targetEntity=HibNodeToken.class)
  @JoinColumn (name = "parent_token_id", nullable=true)
  protected NodeToken parentToken;

  @Column (name="create_date", updatable=false)
  @Temporal (TemporalType.TIMESTAMP)
  protected Date createDate;

  @CollectionOfElements
  @JoinTable( name="wf_process_attr", joinColumns={@JoinColumn( name="process_id")})
  @org.hibernate.annotations.MapKey( columns={@Column(name="name")})
  @Column( name="value")
  protected Map<String, String> attrMap;

  protected ProcessState state;

  @Transient
  protected Env env = null;

  public HibProcess () { /* Default constructor for Hibernate */ }

  public HibProcess (HibGraph graph)
  {
    this.graph = graph;
    this.arcTokens = new LinkedList<ArcToken>();
    this.nodeTokens = new LinkedList<NodeToken>();
    this.state = ProcessState.Created;
    this.createDate = new Date();
    this.attrMap = new HashMap<String, String>();
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  @Override
  public HibGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (HibGraph graph)
  {
    this.graph = graph;
  }

  @Override
  public List<ArcToken> getArcTokens ()
  {
    return arcTokens;
  }

  public void setArcTokens (List<ArcToken> arcTokens)
  {
    this.arcTokens = arcTokens;
  }

  public List<NodeToken> getNodeTokens()
  {
    return nodeTokens;
  }

  public void setNodeTokens( List<NodeToken> nodeTokens )
  {
    this.nodeTokens = nodeTokens;
  }

  @Override
  public NodeToken getParentToken ()
  {
    return parentToken;
  }

  public void setParentToken (NodeToken parentToken)
  {
    this.parentToken = parentToken;
  }

  public Date getCreateDate()
  {
    return createDate;
  }

  public void setCreateDate( Date createDate )
  {
    this.createDate = createDate;
  }

  public Map<String, String> getAttrMap()
  {
    return attrMap;
  }

  public void setAttrMap( Map<String, String> attrMap )
  {
    this.attrMap = attrMap;
  }

  public Env getEnv ()
  {
    if (env == null)
    {
      env = new MapEnv( getAttrMap() );
    }
    return env;
  }

  @Override
  public void addArcToken (ArcToken token)
  {
    getArcTokens().add( token );
  }

  @Override
  public void removeArcToken (ArcToken token)
  {
    getArcTokens().remove( token );
  }

  @Override
  public void addNodeToken (NodeToken token)
  {
    getNodeTokens().add( token );
  }

  @Override
  public void removeNodeToken (NodeToken token)
  {
    getNodeTokens().remove( token );
  }

  @Override
  public ProcessState getState ()
  {
    return state;
  }

  public void setState (ProcessState state)
  {
    this.state = state;
  }

  @Override
  public boolean isCanceled ()
  {
    return state == ProcessState.PendingCancel || state == ProcessState.Canceled;
  }

  @Override
  public boolean isExecuting ()
  {
    return state == ProcessState.Executing;
  }

  @Override
  public boolean hasActiveTokens ()
  {
    return !arcTokens.isEmpty() || !nodeTokens.isEmpty();
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
    if ( !( obj instanceof HibProcess ) ) return false;
    final HibProcess other = (HibProcess)obj;
    if ( id == null )
    {
      if ( other.getId() != null ) return false;
    }
    else if ( !id.equals( other.getId() ) ) return false;
    return true;
  }
}