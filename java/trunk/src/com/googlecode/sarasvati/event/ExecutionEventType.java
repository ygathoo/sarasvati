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
  PROCESS_STARTED ( 2 ),
  PROCESS_COMPLETED( 4 ),
  PROCESS_CANCELED( 8 ),

  NODE_TOKEN_CREATED( 16 ),
  NODE_TOKEN_ACCEPTED( 32 ),
  NODE_TOKEN_EXECUTED( 64 ),
  NODE_TOKEN_DISCARDED( 128 ),
  NODE_TOKEN_SKIPPED( 256 ),
  NODE_TOKEN_COMPLETED( 512 ),
  NODE_TOKEN_BACKTRACKED( 1024 ),

  ARC_TOKEN_CREATED( 2048 ),
  ARC_TOKEN_COMPLETED( 4096 ),
  ARC_TOKEN_BACKTRACKED( 8192 );

  private int eventType;

  private ExecutionEventType (final int eventType)
  {
    this.eventType = eventType;
  }

  public int getEventType ()
  {
    return eventType;
  }

  public boolean isProcessEvent ()
  {
    return this == PROCESS_STARTED ||
           this == PROCESS_COMPLETED ||
           this == PROCESS_CANCELED;
  }

  public boolean isNodeTokenEvent ()
  {
    return this == NODE_TOKEN_CREATED     ||
           this == NODE_TOKEN_ACCEPTED    ||
           this == NODE_TOKEN_EXECUTED    ||
           this == NODE_TOKEN_COMPLETED   ||
           this == NODE_TOKEN_DISCARDED   ||
           this == NODE_TOKEN_SKIPPED     ||
           this == NODE_TOKEN_BACKTRACKED;
  }

  public boolean isArcTokenEvent ()
  {
    return this == ARC_TOKEN_CREATED     ||
           this == ARC_TOKEN_COMPLETED   ||
           this == ARC_TOKEN_BACKTRACKED;
  }
}