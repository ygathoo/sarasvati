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

import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.Process;

public class NodeTokenEvent extends ExecutionEvent
{
  protected NodeToken nodeToken;
  protected GuardResponse guardResponse;

  public static final NodeTokenEvent newCreatedEvent (NodeToken nodeToken)
  {
    return new NodeTokenEvent( ExecutionEventType.NODE_TOKEN_CREATED, nodeToken, null );
  }

  public static final NodeTokenEvent newAcceptedEvent (NodeToken nodeToken, GuardResponse response)
  {
    return new NodeTokenEvent( ExecutionEventType.NODE_TOKEN_ACCEPTED, nodeToken, response );
  }

  public static final NodeTokenEvent newDiscardedEvent (NodeToken nodeToken, GuardResponse response)
  {
    return new NodeTokenEvent( ExecutionEventType.NODE_TOKEN_DISCARDED, nodeToken, response );
  }

  public static final NodeTokenEvent newSkippedEvent (NodeToken nodeToken, GuardResponse response)
  {
    return new NodeTokenEvent( ExecutionEventType.NODE_TOKEN_SKIPPED, nodeToken, response );
  }

  public static final NodeTokenEvent newCompletedEvent (NodeToken nodeToken)
  {
    return new NodeTokenEvent( ExecutionEventType.NODE_TOKEN_COMPLETED, nodeToken, null );
  }

  private NodeTokenEvent (ExecutionEventType eventType, NodeToken nodeToken, GuardResponse guardResponse)
  {
    super( eventType );
    this.nodeToken = nodeToken;
    this.guardResponse = guardResponse;
  }

  @Override
  public NodeToken getNodeToken()
  {
    return nodeToken;
  }

  @Override
  public GuardResponse getGuardResponse()
  {
    return guardResponse;
  }

  @Override
  public Process getProcess()
  {
    return nodeToken.getProcess();
  }
}
