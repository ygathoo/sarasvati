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

import com.googlecode.sarasvati.BaseEngine;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.WorkflowException;
import com.googlecode.sarasvati.event.DefaultExecutionEventQueue;
import com.googlecode.sarasvati.event.ExecutionEvent;
import com.googlecode.sarasvati.event.ExecutionEventQueue;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.event.ExecutionListener;
import com.googlecode.sarasvati.load.GraphLoader;

public class MemEngine extends BaseEngine
{
  protected static final ExecutionEventQueue globalEventQueue = DefaultExecutionEventQueue.newCopyOnWriteListInstance();

  protected MemEngine parentEngine = null;

  @Override
  public MemGraphFactory getFactory ()
  {
    return MemGraphFactory.INSTANCE;
  }

  @Override
  public MemGraphRepository getRepository ()
  {
    return MemGraphRepository.INSTANCE;
  }

  @Override
  public GraphLoader<MemGraph> getLoader()
  {
    return new GraphLoader<MemGraph>( MemGraphFactory.INSTANCE, MemGraphRepository.INSTANCE );
  }

  @Override
  public void fireEvent (ExecutionEvent event)
  {
    globalEventQueue.fireEvent( event );
    event.getProcess().getEventQueue().fireEvent( event );
  }

  @Override
  public void addExecutionListener (ExecutionListener listener, ExecutionEventType... eventTypes)
  {
    globalEventQueue.addListener( this, listener, eventTypes );
  }

  @Override
  public void addExecutionListener (GraphProcess process, ExecutionListener listener, ExecutionEventType... eventTypes)
  {
    process.getEventQueue().addListener( this, listener, eventTypes );
  }

  @Override
  public void removeExecutionListener (ExecutionListener listener, ExecutionEventType... eventTypes)
  {
    globalEventQueue.removeListener( this, listener, eventTypes );
  }

  @Override
  public void removeExecutionListener (GraphProcess process, ExecutionListener listener, ExecutionEventType... eventTypes)
  {
    process.getEventQueue().removeListener( this, listener, eventTypes );
  }

  @Override
  public ExecutionListener getExecutionListenerInstance (String type) throws WorkflowException
  {
    return null;
  }

  @Override
  public MemEngine newEngine (boolean forNested)
  {
    MemEngine engine = new MemEngine ();

    if ( forNested )
    {
      engine.parentEngine = this;
    }
    return engine;
  }

  @Override
  public MemEngine getParentEngine ()
  {
    return parentEngine;
  }
}