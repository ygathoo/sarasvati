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
package com.googlecode.sarasvati.jdbc;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.adapter.NodeAdapterManager;

public class JdbcNode implements Node, HasGeneratedId
{
  protected Long      id;
  protected String    name;
  protected String    type;
  protected String    guard;
  protected boolean   isStart;
  protected boolean   isJoin;

  protected JdbcGraph graph;

  public JdbcNode ()
  {
    // default constructor
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public String getType ()
  {
    return type;
  }

  public void setType (String type)
  {
    this.type = type;
  }

  public String getGuard ()
  {
    return guard;
  }

  public void setGuard (String guard)
  {
    this.guard = guard;
  }

  public boolean isStart ()
  {
    return isStart;
  }

  public void setStart (boolean isStart)
  {
    this.isStart = isStart;
  }

  public boolean isJoin ()
  {
    return isJoin;
  }

  public void setJoin (boolean isJoin)
  {
    this.isJoin = isJoin;
  }

  public JdbcGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (JdbcGraph graph)
  {
    this.graph = graph;
  }

  /**
   * Does nothing by default. Can be overridden by subclasses.
   * @see Node#backtrack(Engine, NodeToken)
   */
  @Override
  public void backtrack (Engine engine, NodeToken token)
  {
    // does nothing by default
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
  @Override
  public <T> T getAdaptor (Class<T> clazz)
  {
    return NodeAdapterManager.getAdaptor( this, clazz );
  }

  @Override
  public boolean isExternal ()
  {
    return false;
  }

  /**
   * Called after this node is persisted to the database. Allows subclasses
   * to persist additional attributes to the database.
   *
   * @param engine The {@link JdbcEngine} to use to work with the database
   */
  public void afterCreate (JdbcEngine engine)
  {
    // does nothing by default.
  }

  /**
   * Called after this node is loaded from the database. Allows subclasses
   * to persist additional attributes to the database.
   *
   * @param engine The {@link JdbcEngine} to use to work with the database
   */
  public void afterLoad (JdbcEngine engine)
  {
    // does nothing by default.
  }
}