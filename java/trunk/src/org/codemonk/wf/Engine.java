/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class Engine
{
  protected abstract NodeToken newNodeToken (Process process, Node node, List<ArcToken> parents);

  protected abstract ArcToken newArcToken (Process process, Arc arc, NodeToken parent);

  protected abstract Process newProcess (Graph graph);

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
    GuardResponse response = token.getNode().guard( process, token );
    switch ( response.getGuardAction() )
    {
      case AcceptToken :
        process.addNodeToken( token );
        token.getNode().execute( this, process, token );
        break;

      case DiscardToken :
        token.markComplete();
        break;

      case SkipNode :
        process.addNodeToken( token );
        completeExecuteNode( process, token, response.getExitArcForSkip() );
        break;
    }
  }

  public void completeExecuteNode (Process process, NodeToken token, String arcName)
  {
    List<Arc> outputArcs = process.getGraph().getOutputArcs( token.getNode(), arcName );

    process.removeNodeToken( token );
    token.markComplete();

    for ( Arc arc : outputArcs )
    {
      executeArc( process, newArcToken( process, arc, token ) );
    }
  }
}