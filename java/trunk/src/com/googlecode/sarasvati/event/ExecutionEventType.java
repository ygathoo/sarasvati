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

public enum ExecutionEventType
{
  PROCESS_CREATED( 1 ),
  PROCESS_STARTED ( 2 ),
  PROCESS_COMPLETED( 4 ),
  PROCESS_CANCELED( 8 ),

  NODE_TOKEN_CREATED( 16 ),
  NODE_TOKEN_ACCEPTED( 32 ),
  NODE_TOKEN_DISCARDED( 64 ),
  NODE_TOKEN_SKIPPED( 128 ),
  NODE_TOKEN_COMPLETED( 256 ),

  ARC_TOKEN_CREATED( 512 ),
  ARC_TOKEN_COMPLETED( 1024 );

  private int eventType;

  private ExecutionEventType (int eventType)
  {
    this.eventType = eventType;
  }

  public int getEventType ()
  {
    return eventType;
  }
}
