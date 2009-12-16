/**
 * Created on Dec 14, 2009
 */
package com.googlecode.sarasvati.join.lang;

import java.util.Set;

import com.googlecode.sarasvati.ArcToken;

class NodeRequiredEvaluator extends AbstractJoinRequirementEvaluator
{
  private final NodeRequired requirement;
  private ArcToken targetToken;

  public NodeRequiredEvaluator (final JoinLangEnv env, final NodeRequired requirement)
  {
    super( env );
    this.requirement = requirement;
  }

  @Override
  protected JoinRequirement getRequirement ()
  {
    return requirement;
  }

  @Override
  public void evaluate ()
  {
    // Check the initiating token first, since we want to be sure to grab that one, if it's applicable
    if ( requirement.getNodeName().equals( getEnv().getInitiatingToken().getArc().getStartNode().getName() ) )
    {
      targetToken = getEnv().getInitiatingToken();
      return;
    }

    for ( ArcToken token : getEnv().getAvailableTokens() )
    {
      if ( requirement.getNodeName().equals( token.getArc().getStartNode().getName() ) )
      {
        targetToken = token;
        return;
      }
    }
  }

  @Override
  public void completeJoinAndContributeTokens (final Set<ArcToken> tokens)
  {
    if ( targetToken != null )
    {
      tokens.add( targetToken );
    }
  }

  @Override
  public boolean isSatisfied ()
  {
    return targetToken != null;
  }

  @Override
  public boolean isInitiatingTokenIncluded ()
  {
    return getEnv().getInitiatingToken().equals( targetToken );
  }
}