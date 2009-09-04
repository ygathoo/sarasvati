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
  /**
   * NOTE: The eventType ids below are used in constructing bitmasks. Since
   *       the bitmasks are stored in the database, these ids must be
   *       stable. If adding a new event type, it must be assigned the next
   *       highest power of 2.
   */
  PROCESS_CREATED ( 1 ),
  PROCESS_STARTED ( 2 ),
  PROCESS_PENDING_COMPLETE(4),
  PROCESS_COMPLETED( 8 ),
  PROCESS_PENDING_CANCEL(16),
  PROCESS_CANCELED( 32 ),

  NODE_TOKEN_CREATED( 64 ),
  NODE_TOKEN_ACCEPTED( 128 ),
  NODE_TOKEN_EXECUTED( 256 ),
  NODE_TOKEN_DISCARDED( 512 ),
  NODE_TOKEN_SKIPPED( 1024 ),
  NODE_TOKEN_COMPLETED( 2048 ),
  NODE_TOKEN_BACKTRACKED( 4096 ),

  ARC_TOKEN_CREATED( 8192 ),
  ARC_TOKEN_PROCESSED( 16384 ),
  ARC_TOKEN_COMPLETED( 32768 ),
  ARC_TOKEN_BACKTRACKED( 65536 );

  private static final int INVERSE_MASK = 0xFFFFFFFF;

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
           this == ARC_TOKEN_PROCESSED   ||
           this == ARC_TOKEN_COMPLETED   ||
           this == ARC_TOKEN_BACKTRACKED;
  }

  public static int toMask (final ExecutionEventType...eventTypes)
  {
    int eventTypeMask = 0;

    for ( ExecutionEventType eventType : eventTypes )
    {
      if ( eventType != null )
      {
        eventTypeMask |= eventType.getEventType();
      }
    }

    return eventTypeMask;
  }

  public static int invertMask (final int mask)
  {
    return mask ^ INVERSE_MASK;
  }
}