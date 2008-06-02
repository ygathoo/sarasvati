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
package org.codemonk.wf;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class WfEngine
{
  protected abstract NodeToken newNodeToken (Process process, Node node, List<ArcToken> parents);

  protected abstract ArcToken newArcToken (Process process, Arc arc, NodeToken parent);

  protected abstract Process newProcess (WfGraph graph);

  public Process startWorkflow (WfGraph graph)
  {
    Process process = newProcess( graph );

    for (Node startNode : graph.getStartNodes() )
    {
      NodeToken startToken = newNodeToken( process, startNode, new ArrayList<ArcToken>(0) );
      executeNode( process, startToken );
    }

    return process;
  }

  protected void executeArc (Process process, ArcToken token)
  {
    if ( !token.getArc().getEndNode().isJoin() )
    {
      completeExecuteArc( process, token.getArc().getEndNode(), token );
    }
    else
    {
      process.addArcToken( token );

      Node targetNode = token.getArc().getEndNode();
      List<Arc> inputs = process.getGraph().getInputArcs( targetNode, token.getArc().getName() );

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

  protected void completeExecuteArc (Process process, Node targetNode, ArcToken ... tokens)
  {
    for ( ArcToken token : tokens )
    {
      process.removeArcToken( token );
      token.markComplete();
    }

    executeNode( process, newNodeToken( process, targetNode, Arrays.asList( tokens ) ) );
  }

  protected void executeNode (Process process, NodeToken token)
  {
    GuardResponse response = token.getNode().guard( this, token );
    switch ( response.getGuardAction() )
    {
      case AcceptToken :
        process.addNodeToken( token );
        token.getNode().execute( this, token );
        break;

      case DiscardToken :
        token.markComplete();
        break;

      case SkipNode :
        process.addNodeToken( token );
        completeExecuteNode( token, response.getExitArcForSkip() );
        break;
    }
  }

  public void completeExecuteNode (NodeToken token, String arcName)
  {
    Process process = token.getProcess();
    List<Arc> outputArcs = process.getGraph().getOutputArcs( token.getNode(), arcName );

    process.removeNodeToken( token );
    token.markComplete();

    for ( Arc arc : outputArcs )
    {
      executeArc( process, newArcToken( process, arc, token ) );
    }
  }
}