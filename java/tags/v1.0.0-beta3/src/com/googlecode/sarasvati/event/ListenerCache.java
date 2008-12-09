package com.googlecode.sarasvati.event;

import java.util.concurrent.ConcurrentHashMap;

import com.googlecode.sarasvati.WorkflowException;

public class ListenerCache
{
  protected ConcurrentHashMap<String, ExecutionListener> listenerCache = new ConcurrentHashMap<String, ExecutionListener>();

  public ExecutionListener getListener (String type)
  {
    ExecutionListener listener = listenerCache.get( type );

    if ( listener == null )
    {
      try
      {
        Class<?> listenerClass = Class.forName( type );
        listener = (ExecutionListener) listenerClass.newInstance();
        listenerCache.put( type, listener );
      }
      catch (Exception e)
      {
        throw new WorkflowException( "Failed to instantiate ExecutionListener of type " + type, e );
      }
    }

    return listener;
  }

  public void ensureContainsListenerType (ExecutionListener listener)
  {
    String type = listener.getClass().getName();

    if ( !listenerCache.contains( type ) )
    {
      listenerCache.put( type, listener );
    }
  }
}
