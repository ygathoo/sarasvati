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

package com.googlecode.sarasvati.hib;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
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

import org.hibernate.Session;
import org.hibernate.annotations.DiscriminatorFormula;
import org.hibernate.annotations.Type;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.adapter.NodeAdapterManager;

@Entity
@Table (name="wf_node")
@Inheritance (strategy=InheritanceType.SINGLE_TABLE)
@DiscriminatorFormula( "(select t.behaviour from wf_node_type t where t.id = type)" )
@DiscriminatorValue( "node" )
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

  @Column (name="is_start")
  @Type (type="yes_no")
  protected boolean start;

  protected String guard;

  public HibNode () { /* Default constructor for Hibernate */ }

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
  public boolean isJoin ()
  {
    return join;
  }

  public void setJoin (boolean join)
  {
    this.join = join;
  }

  @Override
  public boolean isStart()
  {
    return start;
  }

  public void setStart( boolean start )
  {
    this.start = start;
  }

  public String getGuard()
  {
    return guard;
  }

  public void setGuard( String guard )
  {
    this.guard = guard;
  }

  /**
   * This is meaningless for HibNode, since these are never directly
   * external. This maybe wrapped by some node refs which are external
   * and others which aren't.
   */
  @Override
  public boolean isExternal()
  {
    return false;
  }

  /**
   * Does nothing by default. Can be overridden by subclasses.
   * @see Node#backtrack(NodeToken)
   */
  @Override
  public void backtrack (Engine engine, NodeToken token)
  {
    // does nothing by default. Can be overridden by subclasses.
  }

  /**
   * Returns true. Can be overridden by subclasses.
   *
   * @see Node#isBacktrackable(Engine,NodeToken)
   */
  @Override
  public boolean isBacktrackable (Engine engine, NodeToken token)
  {
    return true;
  }

  @Override
  public GuardResponse guard (Engine engine, NodeToken token)
  {
    return engine.evaluateGuard( token, guard );
  }

  @Override
  public void execute (Engine engine, NodeToken token)
  {
    engine.completeExecution( token, Arc.DEFAULT_ARC );
  }

  /**
   * Will use the {@link NodeAdapterManager} to produce an adapter.
   * Subclasses may override this behavior.
   *
   * @see Node#getAdaptor(Class)
   */
  @Override public <T> T getAdaptor (Class<T> clazz)
  {
    return NodeAdapterManager.getAdaptor( this, clazz );
  }

  /**
   * Called when this node should be persisted to the database. May be overridden
   * by subclasses, but then the subclass must either call super.create(...) or
   * call session.save( this ).
   *
   * @param session The hibernate session to use
   */
  public void create (Session session)
  {
    session.save( this );
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
      if ( other.getId() != null ) return false;
    }
    else if ( !id.equals( other.getId() ) ) return false;
    return true;
  }
}