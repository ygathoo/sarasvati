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

package com.googlecode.sarasvati.mem;

import java.util.concurrent.atomic.AtomicLong;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.adapter.NodeAdapterManager;
import com.googlecode.sarasvati.guardlang.GuardLang;

public class MemNode implements Node, Cloneable
{
  protected static AtomicLong idGenerator = new AtomicLong();

  protected Graph graph;

  protected long    id;
  protected String  name;
  protected String  type;
  protected boolean isJoin;
  protected boolean isStart;
  protected String guard;

  protected boolean isExternal;

  public MemNode ()
  {
    /* Default constructor */
  }

  public Long getId ()
  {
    return id;
  }

  public void initId ()
  {
    this.id = idGenerator.incrementAndGet();
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


  @Override
  public Graph getGraph ()
  {
    return graph;
  }

  public void setGraph (Graph graph)
  {
    this.graph = graph;
  }

  @Override
  public String getGuard ()
  {
    return guard;
  }

  public void setGuard (String guard)
  {
    this.guard = guard;
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
  public GuardResponse guard (Engine engine, NodeToken token)
  {
    if ( guard == null || guard.trim().length() == 0 )
    {
      return GuardResponse.ACCEPT_TOKEN_RESPONSE;
    }

    return GuardLang.eval( guard, engine.newGuardEnv( token ) );
  }

  @Override
  public boolean isJoin ()
  {
    return isJoin;
  }

  public void setJoin (boolean isJoin)
  {
    this.isJoin = isJoin;
  }

  @Override
  public boolean isStart ()
  {
    return isStart;
  }

  public void setStart (boolean isStart)
  {
    this.isStart = isStart;
  }

  public boolean isExternal ()
  {
    return isExternal;
  }

  public void setExternal (boolean isExternal)
  {
    this.isExternal = isExternal;
  }

  @Override
  public String getDisplayText ()
  {
    return name;
  }

  @Override
  public MemNode clone ()
  {
    try
    {
      return (MemNode)super.clone();
    }
    catch ( CloneNotSupportedException cnse )
    {
      throw new RuntimeException( cnse );
    }
  }
}