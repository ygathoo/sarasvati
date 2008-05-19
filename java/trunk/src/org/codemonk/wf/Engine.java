/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public abstract class Engine
{
  protected abstract INodeToken newNodeToken (IProcess process, INode node, List<IArcToken> parents);

  protected abstract IArcToken newArcToken (IProcess process, IArc arc, INodeToken parent);

  protected abstract IProcess newProcess (IGraph graph);

  public IProcess startWorkflow (IGraph graph)
  {
    IProcess process = newProcess( graph );

    for (INode startNode : graph.getStartNodes() )
    {
      INodeToken startToken = newNodeToken( process, startNode, new ArrayList<IArcToken>(0) );
      executeNode( process, startToken );
    }

    return process;
  }

  protected void executeArc (IProcess process, IArcToken token)
  {
    if ( !token.getArc().getEndNode().isJoin() )
    {
      completeExecuteArc( process, token.getArc().getEndNode(), token );
    }
    else
    {
      process.addArcToken( token );

      INode targetNode = token.getArc().getEndNode();
      List<IArc> inputs = process.getGraph().getInputArcs( targetNode, token.getArc().getName() );

      IArcToken[] tokens = new IArcToken[inputs.size()];
      int tokensFound = 0;

      for ( IArc arc : inputs )
      {
        for ( IArcToken arcToken : process.getArcTokens() )
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

  protected void completeExecuteArc (IProcess process, INode targetNode, IArcToken ... tokens)
  {
    for ( IArcToken token : tokens )
    {
      process.removeArcToken( token );
      token.markComplete();
    }

    executeNode( process, newNodeToken( process, targetNode, Arrays.asList( tokens ) ) );
  }

  protected void executeNode (IProcess process, INodeToken token)
  {
    IGuardResponse response = token.getNode().guard( process, token );
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

  public void completeExecuteNode (IProcess process, INodeToken token, String arcName)
  {
    List<IArc> outputArcs = process.getGraph().getOutputArcs( token.getNode(), arcName );

    process.removeNodeToken( token );
    token.markComplete();

    for ( IArc arc : outputArcs )
    {
      executeArc( process, newArcToken( process, arc, token ) );
    }
  }
}