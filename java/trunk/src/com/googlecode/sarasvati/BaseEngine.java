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
/**
 * Created on Apr 25, 2008
 */
package com.googlecode.sarasvati;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 *
 * @author Paul Lorenz
 */
public abstract class BaseEngine implements Engine
{
  public Process startWorkflow (Graph graph)
  {
    Process process = newProcess( graph );

    for (Node startNode : graph.getStartNodes() )
    {
      NodeToken startToken = newNodeToken( process, startNode, new ArrayList<ArcToken>(0) );
      executeNode( process, startToken );
    }

    return process;
  }

  private void executeArc (Process process, ArcToken token)
  {
    if ( !token.getArc().getEndNode().isJoin() )
    {
      completeExecuteArc( process, token.getArc().getEndNode(), token );
    }
    else
    {
      process.addArcToken( token );

      Node targetNode = token.getArc().getEndNode();
      List<? extends Arc> inputs = process.getGraph().getInputArcs( targetNode, token.getArc().getName() );

      ArcToken[] tokens = new ArcToken[inputs.size()];
      int tokensFound = 0;

      for ( Arc arc : inputs )
      {
        for ( ArcToken arcToken : process.getArcTokens() )
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

  private void completeExecuteArc (Process process, Node targetNode, ArcToken ... tokens)
  {
    for ( ArcToken token : tokens )
    {
      process.removeArcToken( token );
      token.markComplete( this );
    }

    executeNode( process, newNodeToken( process, targetNode, Arrays.asList( tokens ) ) );
  }

  protected void executeNode (Process process, NodeToken token)
  {
    GuardResponse response = token.getNode().guard( this, token );
    token.recordGuardAction( this, response.getGuardAction() );

    switch ( response.getGuardAction() )
    {
      case AcceptToken :
        process.addNodeToken( token );
        token.getNode().execute( this, token );
        break;

      case DiscardToken :
        token.markComplete( this );
        break;

      case SkipNode :
        process.addNodeToken( token );
        completeExecution( token, response.getExitArcForSkip() );
        break;
    }
  }

  public void completeExecution (NodeToken token, String arcName)
  {
    System.out.println( "Stack depth: " + new Exception().getStackTrace().length );
    Process process = token.getProcess();
    List<? extends Arc> outputArcs = process.getGraph().getOutputArcs( token.getNode(), arcName );

    process.removeNodeToken( token );
    token.markComplete( this );

    for ( Arc arc : outputArcs )
    {
      executeArc( process, newArcToken( process, arc, token ) );
    }
  }
}