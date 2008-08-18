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

public class ExecutionEventDispatcher
{
  private List<ExecutionListenerWrapper> listeners = new ArrayList<ExecutionListenerWrapper>();

  public static ExecutionEventDispatcher newArrayListInstance ()
  {
    return new ExecutionEventDispatcher( new ArrayList<ExecutionListenerWrapper>() );
  }

  public static ExecutionEventDispatcher newCopyOnWriteListInstance ()
  {
    return new ExecutionEventDispatcher( new CopyOnWriteArrayList<ExecutionListenerWrapper>() );
  }

  private ExecutionEventDispatcher(List<ExecutionListenerWrapper> listeners)
  {
    this.listeners = listeners;
  }

  public void addExecutionListener (ExecutionListener listener, ExecutionEventType...eventTypes)
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
