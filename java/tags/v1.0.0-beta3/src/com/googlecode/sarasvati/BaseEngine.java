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
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.googlecode.sarasvati.event.ArcTokenEvent;
import com.googlecode.sarasvati.event.NodeTokenEvent;
import com.googlecode.sarasvati.event.ProcessEvent;
import com.googlecode.sarasvati.rubric.RubricInterpreter;
import com.googlecode.sarasvati.rubric.env.DefaultRubricEnv;
import com.googlecode.sarasvati.rubric.env.DefaultRubricFunctionRepository;
import com.googlecode.sarasvati.rubric.env.RubricEnv;
import com.googlecode.sarasvati.script.ScriptEnv;

/**
 *
 * @author Paul Lorenz
 */
public abstract class BaseEngine implements Engine
{
  protected boolean arcExecutionStarted = false;
  protected List<ArcToken> asyncQueue = new LinkedList<ArcToken>();

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

    arcExecutionStarted = true;

    try
    {
      for ( Node startNode : process.getGraph().getStartNodes() )
      {
        NodeToken startToken = getFactory().newNodeToken( process, startNode, new ArrayList<ArcToken>(0) );
        process.addNodeToken( startToken );
        executeNode( process, startToken );
      }
    }
    finally
    {
      executeQueuedArcTokens( process );
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
      Engine engine = getParentEngine() == null ? newEngine( false ) : getParentEngine();
      engine.completeExecution( parentToken, Arc.DEFAULT_ARC );
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
    NodeToken nodeToken = getFactory().newNodeToken( process, targetNode, Arrays.asList( tokens ) );
    process.addNodeToken( nodeToken );
    fireEvent( NodeTokenEvent.newCreatedEvent( this, nodeToken ) );

    for ( ArcToken token : tokens )
    {
      process.removeActiveArcToken( token );
      token.markComplete( this, nodeToken );
      fireEvent( ArcTokenEvent.newCompletedEvent( this, token ) );
    }

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
    completeNodeToken( token, arcName, true );
  }

  protected void completeNodeToken (NodeToken token, String arcName, boolean asynchronous)
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
      token.getChildTokens().add(  arcToken );
      fireEvent( ArcTokenEvent.newCreatedEvent( this, arcToken ) );

      if ( asynchronous && arcExecutionStarted )
      {
        asyncQueue.add( arcToken );
      }
      else
      {
        process.enqueueArcTokenForExecution( arcToken );
      }
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

    completeNodeToken( token, arcName, false );

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

      while ( !asyncQueue.isEmpty() )
      {
        process.enqueueArcTokenForExecution( asyncQueue.remove( 0 ) );
      }
    }
  }

  private void checkForCompletion (GraphProcess process)
  {
    if ( !process.hasActiveTokens() && process.isArcTokenQueueEmpty() && asyncQueue.isEmpty() )
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

  @Override
  public void addNodeType(String type, Class<? extends Node> nodeClass)
  {
    getFactory().addType(type, nodeClass);
  }

  public void backtrack (NodeToken token)
  {
    Set<NodeToken> leaves = new HashSet<NodeToken>();
    Set<NodeToken> processed = new HashSet<NodeToken>();
    List<NodeToken> queue = new LinkedList<NodeToken>();

    queue.add( token );

    while ( !queue.isEmpty() )
    {
      NodeToken current = queue.remove( 0 );
      if ( !processed.contains( current ) )
      {
        processed.add( current );

        if ( !current.getNode().isBacktrackable( current ) )
        {
          throw new WorkflowException( "Can not backtrack node" );
        }

        for ( ArcToken childArcToken : current.getChildTokens() )
        {
          NodeToken child = childArcToken.getChildToken();
          (child.getChildTokens().isEmpty() ? leaves : queue).add( child );
        }
      }
    }

    queue.addAll( leaves );

    while ( !queue.isEmpty() )
    {
      NodeToken current = queue.remove( 0 );
      current.getNode().backtrack(  current );

      if ( !current.isComplete() )
      {
        current.markComplete( this );
      }

      for ( ArcToken arcToken : current.getParentTokens() )
      {
        NodeToken parent = arcToken.getParentToken();
        if ( processed.contains( parent ) )
        {
          queue.add( parent );
        }
      }
    }
  }

  @Override
  public RubricEnv newRubricEnv (NodeToken token)
  {
    return new DefaultRubricEnv( this, token, DefaultRubricFunctionRepository.getGlobalInstance() );
  }

  @Override
  public GuardResponse evaluateGuard (NodeToken token, String guard)
  {
    if ( guard == null || guard.trim().length() == 0 )
    {
      return GuardResponse.ACCEPT_TOKEN_RESPONSE;
    }

    return (GuardResponse) RubricInterpreter.compile( guard ).eval( newRubricEnv( token ) );
  }
}