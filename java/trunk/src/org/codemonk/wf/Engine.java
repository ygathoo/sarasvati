/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.ArrayList;
import java.util.List;

public abstract class Engine
{
  protected abstract INodeToken newNodeToken (IProcess process, INode node, List<IArcToken> parents);

  protected abstract IArcToken newArcToken (IProcess process, IArc arc, INodeToken parent);

  protected abstract IProcess startWorkflow (IGraph graph);

  public void accept (IProcess process, IArcToken token)
  {
    if ( token.getArc().getEndNode().isJoin() )
    {
      acceptJoin( process, token );
    }
    else
    {
      acceptSingle( process, token );
    }
  }

  protected void acceptSingle (IProcess process, IArcToken token)
  {
    List<IArcToken> tokens = new ArrayList<IArcToken>(1);
    tokens.add( token );

    finishAccept( process, token.getArc().getEndNode(), tokens );
  }

  protected void acceptJoin (IProcess process, IArcToken token)
  {
    INode targetNode = token.getArc().getEndNode();
    List<IArc> inputs = process.getGraph().getInputArcs( targetNode, token.getArc().getName() );

    List<IArcToken> tokens = new ArrayList<IArcToken>(inputs.size());
    tokens.add( token );

    for ( IArc arc : inputs )
    {
      if ( arc.equals( token.getArc() ) )
      {
        continue;
      }

      for ( IArcToken arcToken : process.getArcTokens() )
      {
        if ( arcToken.getArc().equals( arc ) )
        {
          tokens.add( arcToken );
          break;
        }
      }
    }

    boolean allInputsPresent = tokens.size() == inputs.size();

    if ( allInputsPresent )
    {
      finishAccept( process, targetNode, tokens );
    }
    else
    {
      process.addArcToken( token );
    }
  }

  protected void finishAccept (IProcess process, INode targetNode, List<IArcToken> tokens)
  {
    for ( IArcToken arcToken : tokens )
    {
      process.removeArcToken( arcToken );
      arcToken.markComplete();
    }

    acceptWithGuard( process, newNodeToken( process, targetNode, tokens ) );
  }

  public void acceptWithGuard (IProcess process, INodeToken token)
  {
    switch ( token.getNode().guard( process, token ) )
    {
      case AcceptToken :
        token.getNode().execute( this, process, token );
        break;

      case DiscardToken :
        token.markComplete();
        break;

      case SkipNode :
        completeExecution( process, token, "" );
        break;
    }
  }

  public void completeExecution (IProcess process, INodeToken token, String arcName)
  {
    List<IArc> outputArcs = process.getGraph().getOutputArcs( token.getNode(), arcName );

    token.markComplete();

    for ( IArc arc : outputArcs )
    {
      accept( process, newArcToken( process, arc, token ) );
    }
  }
}