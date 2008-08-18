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

/**
 * An ExecutionEventQueue manages a set of {@link ExecutionListener}s. It handles
 * adding and removing listeners as well as sending events to the appropriate
 * listeners.
 *
 * @author Paul Lorenz
 */
public interface ExecutionEventQueue
{
  void addListener (Engine engine, ExecutionListener listener, ExecutionEventType... eventTypes);

  /**
   * Sends the event to the appropriate listeners.
   *
   * @param event The execution event
   */
  void fireEvent (ExecutionEvent event);
}