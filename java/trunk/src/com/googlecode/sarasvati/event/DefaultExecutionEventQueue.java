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
package com.googlecode.sarasvati.event;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import com.googlecode.sarasvati.Engine;

public class DefaultExecutionEventQueue implements ExecutionEventQueue
{
  private List<ExecutionListenerWrapper> listeners;

  public static ExecutionEventQueue newArrayListInstance ()
  {
    return new DefaultExecutionEventQueue( new ArrayList<ExecutionListenerWrapper>() );
  }

  public static ExecutionEventQueue newCopyOnWriteListInstance ()
  {
    return new DefaultExecutionEventQueue( new CopyOnWriteArrayList<ExecutionListenerWrapper>() );
  }

  private DefaultExecutionEventQueue(List<ExecutionListenerWrapper> listeners)
  {
    this.listeners = listeners;
  }

  /**
   * @see com.googlecode.sarasvati.event.ExecutionEventQueue#addListener(com.googlecode.sarasvati.Engine, com.googlecode.sarasvati.event.ExecutionListener, com.googlecode.sarasvati.event.ExecutionEventType[])
   */
  public synchronized void addListener (Engine engine, ExecutionListener listener, ExecutionEventType...eventTypes)
  {
    if ( eventTypes == null || listener == null)
    {
      return;
    }

    for ( ExecutionEventType eventType : eventTypes )
    {
      if (eventType != null)
      {
        listeners.add( new ExecutionListenerWrapper( eventType, listener ) );
      }
    }
  }

  /**
   * @see com.googlecode.sarasvati.event.ExecutionEventQueue#fireEvent(com.googlecode.sarasvati.event.ExecutionEvent)
   */
  public void fireEvent (ExecutionEvent event)
  {
    for (ExecutionListenerWrapper wrapper : listeners )
    {
      if ( event.getEventType() == wrapper.getEventType() )
      {
        wrapper.notify( event );
      }
    }
  }
}
