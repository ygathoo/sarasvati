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

import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Process;

public class ProcessEvent extends ExecutionEvent
{
  public static final ProcessEvent newStartedEvent (Engine engine, Process process)
  {
    return new ProcessEvent( engine, ExecutionEventType.PROCESS_STARTED, process );
  }

  public static final ProcessEvent newCompletedEvent (Engine engine, Process process)
  {
    return new ProcessEvent( engine, ExecutionEventType.PROCESS_COMPLETED, process );
  }

  public static final ProcessEvent newCanceledEvent (Engine engine, Process process)
  {
    return new ProcessEvent( engine, ExecutionEventType.PROCESS_CANCELED, process );
  }

  private Process process;

  public ProcessEvent (Engine engine, ExecutionEventType eventType, Process process)
  {
    super( engine, eventType );
    this.process = process;
  }

  @Override
  public Process getProcess ()
  {
    return process;
  }
}
