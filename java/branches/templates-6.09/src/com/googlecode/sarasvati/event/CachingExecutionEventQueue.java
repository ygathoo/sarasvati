package com.googlecode.sarasvati.event;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import com.googlecode.sarasvati.Engine;

public class CachingExecutionEventQueue extends DefaultExecutionEventQueue
{
  public static ExecutionEventQueue newArrayListInstance ()
  {
    return new CachingExecutionEventQueue( new ArrayList<ExecutionListenerWrapper>() );
  }

  public static ExecutionEventQueue newCopyOnWriteListInstance ()
  {
    return new CachingExecutionEventQueue( new CopyOnWriteArrayList<ExecutionListenerWrapper>() );
  }

  protected static final ListenerCache       listenerCache    = new ListenerCache();

  CachingExecutionEventQueue (List<ExecutionListenerWrapper> listeners)
  {
    super( listeners );
  }

  @Override
  public synchronized void addListener (Engine engine, ExecutionListener listener, ExecutionEventType... eventTypes)
  {
    listenerCache.ensureContainsListenerType( listener );
    super.addListener( engine, listener, eventTypes );
  }

  public static ListenerCache getListenerCache ()
  {
    return listenerCache;
  }
}
