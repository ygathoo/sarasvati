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

import com.googlecode.sarasvati.NonRecursiveEngine;
import com.googlecode.sarasvati.Process;
import com.googlecode.sarasvati.event.ExecutionEvent;
import com.googlecode.sarasvati.event.ExecutionEventDispatcher;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.event.ExecutionListener;

public class MemEngine extends NonRecursiveEngine
{
  protected static final ExecutionEventDispatcher globalEventDispatcher = ExecutionEventDispatcher.newCopyOnWriteListInstance();

  @Override
  public MemGraphFactory getFactory()
  {
    return MemGraphFactory.INSTANCE;
  }

  @Override
  public MemGraphRepository getRepository()
  {
    return MemGraphRepository.INSTANCE;
  }

  @Override
  public void fireEvent(ExecutionEvent event)
  {
    globalEventDispatcher.fireEvent( event );
    event.getProcess().getEventDispatcher().fireEvent( event );
  }

  @Override
  public void addExecutionListener(Process process, ExecutionListener listener, ExecutionEventType... eventTypes)
  {
    ExecutionEventDispatcher eventDispatcher = process == null ? globalEventDispatcher : process.getEventDispatcher();
    eventDispatcher.addExecutionListener( listener, eventTypes );
  }
}