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

import java.util.LinkedList;
import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.MapEnv;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.Process;
import com.googlecode.sarasvati.ProcessState;
import com.googlecode.sarasvati.event.DefaultExecutionEventQueue;
import com.googlecode.sarasvati.event.ExecutionEventQueue;

public class MemProcess implements Process
{
  protected long tokenCounter = 0;

  protected Graph graph;
  protected List<ArcToken> arcTokens = new LinkedList<ArcToken>();
  protected List<NodeToken> nodeTokens = new LinkedList<NodeToken>();
  protected ProcessState state;

  protected Env env = new MapEnv();

  protected NodeToken parentToken;

  protected ExecutionEventQueue eventQueue = DefaultExecutionEventQueue.newArrayListInstance();

  public MemProcess (Graph graph)
  {
    this.graph = graph;
    this.state = ProcessState.Created;
  }

  @Override
  public void addArcToken (ArcToken token)
  {
    arcTokens.add( token );
  }

  @Override
  public void addNodeToken (NodeToken token)
  {
    nodeTokens.add( token );
  }

  @Override
  public List<? extends ArcToken> getArcTokens ()
  {
    return arcTokens;
  }

  @Override
  public List<? extends NodeToken> getNodeTokens ()
  {
    return nodeTokens;
  }

  @Override
  public Env getEnv ()
  {
    return env;
  }

  @Override
  public Graph getGraph ()
  {
    return graph;
  }

  public NodeToken getParentToken ()
  {
    return parentToken;
  }

  public void setParentToken (NodeToken parentToken)
  {
    this.parentToken = parentToken;
  }

  @Override
  public void removeArcToken (ArcToken token)
  {
    arcTokens.remove( token );
  }

  @Override
  public void removeNodeToken (NodeToken token)
  {
    nodeTokens.remove( token );
  }

  @Override
  public ProcessState getState ()
  {
    return state;
  }

  @Override
  public void setState (ProcessState state)
  {
    this.state = state;
  }

  @Override
  public boolean isCanceled()
  {
    return state == ProcessState.PendingCancel || state == ProcessState.Canceled;
  }

  @Override
  public boolean isComplete()
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
    return !arcTokens.isEmpty() || !nodeTokens.isEmpty();
  }

  public synchronized long nextTokenId ()
  {
    return tokenCounter++;
  }

  @Override
  public ExecutionEventQueue getEventQueue ()
  {
    return eventQueue;
  }
}