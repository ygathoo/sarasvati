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
package com.googlecode.sarasvati;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.googlecode.sarasvati.event.ArcTokenEvent;
import com.googlecode.sarasvati.event.NodeTokenEvent;
import com.googlecode.sarasvati.event.ProcessEvent;
import com.googlecode.sarasvati.script.ScriptEnv;

/**
 *
 * @author Paul Lorenz
 */
public abstract class BaseEngine implements Engine
{
  protected boolean arcExecutionStarted = false;

  @Override
  public GraphProcess startProcess (Graph graph)
  {
    GraphProcess process = getFactory().newProcess( graph );
    startProcess( process );
    return process;
  }

  @Override
  public void startProcess (GraphProcess process)
  {
    process.setState( ProcessState.Executing );
    fireEvent( ProcessEvent.newStartedEvent( this, process ) );

    for ( Node startNode : process.getGraph().getStartNodes() )
    {
      NodeToken startToken = getFactory().newNodeToken( process, startNode, new ArrayList<ArcToken>(0) );
      process.addNodeToken( startToken );
      executeNode( process, startToken );
    }

    if ( process.isExecuting() )
    {
      checkForCompletion( process );
    }
  }

  @Override
  public void cancelProcess (GraphProcess process)
  {
    process.setState( ProcessState.PendingCancel );
    fireEvent( ProcessEvent.newCanceledEvent( this, process ) );
    finalizeCancel( process );
  }

  @Override
  public void finalizeComplete (GraphProcess process)
  {
    process.setState( ProcessState.Completed );

    NodeToken parentToken = process.getParentToken();
    if ( parentToken != null )
    {
      completeExecution( parentToken, Arc.DEFAULT_ARC );
    }
  }

  @Override
  public void finalizeCancel (GraphProcess process)
  {
    process.setState( ProcessState.Canceled );
  }

  private void executeArc (GraphProcess process, ArcToken token)
  {
    token.markProcessed( this );
    if ( !token.getArc().getEndNode().isJoin() )
    {
      completeExecuteArc( process, token.getArc().getEndNode(), token );
    }
    else
    {
      process.addActiveArcToken( token );

      Node targetNode = token.getArc().getEndNode();
      List<? extends Arc> inputs = process.getGraph().getInputArcs( targetNode, token.getArc().getName() );

      ArcToken[] tokens = new ArcToken[inputs.size()];
      int tokensFound = 0;

      for ( Arc arc : inputs )
      {
        for ( ArcToken arcToken : process.getActiveArcTokens() )
        {
          if ( arcToken.getArc().equals( arc ) )
          {
            tokens[tokensFound++] = arcToken;
            break;
          }
        }
      }

      if ( tokensFound == tokens.length )
      {
        completeExecuteArc( process, targetNode, tokens );
      }
    }
  }

  private void completeExecuteArc (GraphProcess process, Node targetNode, ArcToken ... tokens)
  {
    for ( ArcToken token : tokens )
    {
      process.removeActiveArcToken( token );
      token.markComplete( this );
      fireEvent( ArcTokenEvent.newCompletedEvent( this, token ) );
    }

    NodeToken nodeToken = getFactory().newNodeToken( process, targetNode, Arrays.asList( tokens ) );
    process.addNodeToken( nodeToken );
    fireEvent( NodeTokenEvent.newCreatedEvent( this, nodeToken ) );
    executeNode( process, nodeToken );
  }

  protected void executeNode (GraphProcess process, NodeToken token)
  {
    GuardResponse response = token.getNode().guard( this, token );
    token.recordGuardAction( this, response.getGuardAction() );

    switch ( response.getGuardAction() )
    {
      case AcceptToken :
        process.addActiveNodeToken( token );
        fireEvent( NodeTokenEvent.newAcceptedEvent( this, token ) );
        token.getNode().execute( this, token );
        break;

      case DiscardToken :
        token.markComplete( this );
        fireEvent( NodeTokenEvent.newDiscardedEvent( this, token ) );
        break;

      case SkipNode :
        process.addActiveNodeToken( token );
        fireEvent( NodeTokenEvent.newSkippedEvent( this, token, response.getExitArcForSkip() ) );
        completeExecution( token, response.getExitArcForSkip() );
        break;
    }
  }

  @Override
  public void completeAsynchronous (NodeToken token, String arcName)
  {
    GraphProcess process = token.getProcess();

    if ( !process.isExecuting() || token.isComplete() )
    {
      return;
    }

    process.removeActiveNodeToken( token );
    token.markComplete( this );

    // If the node was skipped, we already sent a 'skipped' event and don't want to
    // send another 'completed' event.
    if ( token.getGuardAction() != GuardAction.SkipNode )
    {
      fireEvent( NodeTokenEvent.newCompletedEvent( this, token, arcName ) );
    }

    for ( Arc arc : process.getGraph().getOutputArcs( token.getNode(), arcName ) )
    {
      ArcToken arcToken = getFactory().newArcToken( process, arc, token );
      fireEvent( ArcTokenEvent.newCreatedEvent( this, arcToken ) );
      process.enqueueArcTokenForExecution( arcToken );
    }
  }

  @Override
  public void completeExecution (NodeToken token, String arcName)
  {
    GraphProcess process = token.getProcess();

    if ( !process.isExecuting() )
    {
      return;
    }

    completeAsynchronous( token, arcName );

    if ( !arcExecutionStarted )
    {
      executeQueuedArcTokens( process );
    }
  }

  @Override
  public void executeQueuedArcTokens (GraphProcess process)
  {
    arcExecutionStarted = true;

    try
    {
      while ( !process.isArcTokenQueueEmpty() )
      {
        executeArc( process, process.dequeueArcTokenForExecution() );
      }
      checkForCompletion( process );
    }
    finally
    {
      arcExecutionStarted = false;
    }
  }

  private void checkForCompletion (GraphProcess process)
  {
    if ( !process.hasActiveTokens() && process.isArcTokenQueueEmpty() )
    {
      process.setState( ProcessState.PendingCompletion );
      fireEvent( ProcessEvent.newCompletedEvent( this, process ) );
      finalizeComplete( process );
    }
  }

  @Override
  public void setupScriptEnv (ScriptEnv env, NodeToken token)
  {
    env.addVariable( "engine", this );
    env.addVariable( "token", token );
  }
}