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
package com.googlecode.sarasvati.jdbc;

import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.persistence.Transient;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.MapEnv;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.ProcessState;
import com.googlecode.sarasvati.event.CachingExecutionEventQueue;
import com.googlecode.sarasvati.event.ExecutionEvent;
import com.googlecode.sarasvati.event.ExecutionEventQueue;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.event.ExecutionListener;
import com.googlecode.sarasvati.event.ListenerCache;
import com.googlecode.sarasvati.hib.HibProcessListener;

public class JdbcGraphProcess implements GraphProcess, JdbcObject
{
  protected Long id;

  protected JdbcGraph graph;

  protected ProcessState state;

  protected JdbcNodeToken parentToken;

  protected Date createDate;
  protected Integer version;

  protected Set<NodeToken> nodeTokens;
  protected Set<ArcToken>  activeArcTokens;
  protected Set<NodeToken> activeNodeTokens;
  protected List<ArcToken>  executionQueue;

  protected List<HibProcessListener>  listeners;

  protected Map<String, String> attrMap;

  @Transient
  protected Env env = null;

  @Transient
  protected ExecutionEventQueue eventQueue = new ExecutionEventQueue()
  {
    @Override
    public void fireEvent (ExecutionEvent event)
    {
      initEventQueue( event.getEngine() );
      eventQueue.fireEvent( event );
    }

    @Override
    public void addListener (Engine engine, ExecutionListener listener, ExecutionEventType... eventTypes)
    {
      initEventQueue( engine );
      eventQueue.addListener( engine, listener, eventTypes );
    }

    @Override
    public void removeListener(Engine engine, ExecutionListener listener, ExecutionEventType... eventTypes)
    {
      initEventQueue( engine );
      eventQueue.removeListener( engine, listener, eventTypes );
    }

    private void initEventQueue (Engine engine)
    {
      ExecutionEventQueue newEventQueue = CachingExecutionEventQueue.newArrayListInstance();
      ListenerCache cache = CachingExecutionEventQueue.getListenerCache();

      for ( HibProcessListener listener : getListeners() )
      {
        newEventQueue.addListener( engine,
                                   cache.getListener( listener.getType() ),
                                   listener.getEventType() );
      }

      eventQueue = newEventQueue;
    }
  };

  public JdbcGraphProcess (JdbcGraph graph, JdbcNodeToken parentToken)
  {
    this( null, graph, ProcessState.Created, parentToken, new Date(), 1 );
  }

  public JdbcGraphProcess (final Long id,
                           final JdbcGraph graph,
                           final ProcessState state,
                           final JdbcNodeToken parentToken,
                           final Date createDate,
                           final int version)
  {
    this.id = id;
    this.graph = graph;
    this.state = state;
    this.parentToken = parentToken;
    this.createDate = createDate;
    this.version = version;

    this.nodeTokens = new HashSet<NodeToken>();
    this.activeArcTokens = new HashSet<ArcToken>();
    this.activeNodeTokens = new HashSet<NodeToken>();
    this.executionQueue = new LinkedList<ArcToken>();
    this.listeners = new LinkedList<HibProcessListener>();

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
  public JdbcGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (JdbcGraph graph)
  {
    this.graph = graph;
  }

  @Override
  public Set<NodeToken> getNodeTokens ()
  {
    return nodeTokens;
  }

  public void setNodeTokens (Set<NodeToken> nodeTokens)
  {
    this.nodeTokens = nodeTokens;
  }

  @Override
  public Set<ArcToken> getActiveArcTokens ()
  {
    return activeArcTokens;
  }

  public void setActiveArcTokens (Set<ArcToken> activeArcTokens)
  {
    this.activeArcTokens = activeArcTokens;
  }

  public Set<NodeToken> getActiveNodeTokens ()
  {
    return activeNodeTokens;
  }

  public void setActiveNodeTokens (Set<NodeToken> activeNodeTokens)
  {
    this.activeNodeTokens = activeNodeTokens;
  }

  public List<ArcToken> getExecutionQueue ()
  {
    return executionQueue;
  }

  public void setExecutionQueue (List<ArcToken> executionQueue)
  {
    this.executionQueue = executionQueue;
  }

  @Override
  public ArcToken dequeueArcTokenForExecution ()
  {
    return executionQueue.remove( 0 );
  }

  @Override
  public void enqueueArcTokenForExecution (ArcToken token)
  {
    executionQueue.add( token );
  }

  @Override
  public boolean isArcTokenQueueEmpty ()
  {
    return executionQueue.isEmpty();
  }

  public List<HibProcessListener> getListeners ()
  {
    return listeners;
  }

  public void setListeners (List<HibProcessListener> listeners)
  {
    this.listeners = listeners;
  }

  public void setEnv (Env env)
  {
    this.env = env;
  }

  @Override
  public JdbcNodeToken getParentToken ()
  {
    return parentToken;
  }

  public void setParentToken (JdbcNodeToken parentToken)
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

  public Integer getVersion ()
  {
    return version;
  }

  public void setVersion (Integer version)
  {
    this.version = version;
  }

  public Map<String, String> getAttrMap ()
  {
    return attrMap;
  }

  public void setAttrMap (Map<String, String> attrMap)
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
  public void addNodeToken (NodeToken token)
  {
    nodeTokens.add( token );
  }

  @Override
  public void addActiveArcToken (ArcToken token)
  {
    activeArcTokens.add( token );
  }

  @Override
  public void removeActiveArcToken (ArcToken token)
  {
    activeArcTokens.remove( token );
  }

  @Override
  public void addActiveNodeToken (NodeToken token)
  {
    activeNodeTokens.add( token );
  }

  @Override
  public void removeActiveNodeToken (NodeToken token)
  {
    activeNodeTokens.remove( token );
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
  public boolean isComplete ()
  {
    return state == ProcessState.PendingCompletion || state == ProcessState.Completed;
  }

  @Override
  public boolean isExecuting ()
  {
    return state == ProcessState.Executing;
  }

  @Override
  public boolean hasActiveTokens ()
  {
    return !activeArcTokens.isEmpty() || !activeNodeTokens.isEmpty();
  }

  @Override
  public ExecutionEventQueue getEventQueue ()
  {
    return eventQueue;
  }

  @Override
  public boolean isMutable ()
  {
    return true;
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
    if ( !( obj instanceof JdbcGraphProcess ) ) return false;
    final JdbcGraphProcess other = (JdbcGraphProcess)obj;
    if ( id == null )
    {
      if ( other.getId() != null ) return false;
    }
    else if ( !id.equals( other.getId() ) ) return false;
    return true;
  }
}