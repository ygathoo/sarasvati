/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.ArrayList;
import java.util.List;

public abstract class Engine
{
  abstract void markTokenComplete (IArcToken token);

  abstract void markTokenComplete (INodeToken token);

  abstract INodeToken newNodeToken (INode node, List<IArcToken> parents);

  abstract IArcToken newArcToken (IArc arc, INodeToken parent);

  public void acceptWithGuard (WfRun wfRun, INodeToken token)
  {
    switch ( token.getNode().guard( wfRun, token ) )
    {
      case AcceptToken :
        token.getNode().execute( this, wfRun, token );
        break;

      case DiscardToken :
        markTokenComplete( token );
        break;

      case SkipNode :
        completeExecution( wfRun, token, "" );
        break;
    }
  }

  public void accept (WfRun wfRun, IArcToken token)
  {
    if ( token.getArc().getEndNode().isJoin() )
    {
      acceptSingle( wfRun, token );
    }
    else
    {
      acceptJoin( wfRun, token );
    }
  }

  protected void acceptSingle (WfRun wfRun, IArcToken token)
  {
    List<IArcToken> tokens = new ArrayList<IArcToken>(1);
    tokens.add( token );

    finishAccept( wfRun, token.getArc().getEndNode(), tokens );
  }

  protected void acceptJoin (WfRun wfRun, IArcToken token)
  {
    INode targetNode = token.getArc().getEndNode();
    List<IArc> inputs = wfRun.getGraph().getInputArcs( targetNode, token.getArc().getName() );

    List<IArcToken> tokens = new ArrayList<IArcToken>(inputs.size());
    tokens.add( token );

    for ( IArc arc : inputs )
    {
      if ( arc.equals( token.getArc() ) )
      {
        continue;
      }

      for ( IArcToken arcToken : wfRun.getArcTokens() )
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
      finishAccept( wfRun, targetNode, tokens );
    }
    else
    {
      wfRun.addArcToken( token );
    }
  }

  protected void finishAccept (WfRun wfRun, INode targetNode, List<IArcToken> tokens)
  {
    for ( IArcToken arcToken : tokens )
    {
      wfRun.removeArcToken( arcToken );
      markTokenComplete( arcToken );
    }

    INodeToken newToken = newNodeToken( targetNode, tokens );
    wfRun.addNodeToken( newToken );
    acceptWithGuard( wfRun, newToken );
  }

  public void completeExecution (WfRun wfRun, INodeToken token, String arcName)
  {
    List<IArc> outputArcs = wfRun.getGraph().getOutputArcs( token.getNode(), arcName );

    markTokenComplete( token );
    wfRun.removeNodeToken( token );

    for ( IArc arc : outputArcs )
    {
      accept( wfRun, newArcToken( arc, token ) );
    }
  }
}