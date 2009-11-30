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

    Copyright 2009 Paul Lorenz
*/
package com.googlecode.sarasvati.join.lang;

import java.util.ArrayList;
import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.rubric.env.PredicateEnv;

public class JoinLangEnvImpl implements JoinLangEnv
{
  private final PredicateEnv predicateEnv;
  private final ArcToken initiatingToken;
  private List<ArcToken> availableTokens = null;
  private boolean isApplicable;
  private boolean isOptional;
  private boolean isRequired;

  public JoinLangEnvImpl (final ArcToken initiatingToken, final PredicateEnv predicateEnv)
  {
    this.initiatingToken = initiatingToken;
    this.predicateEnv = predicateEnv;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#getAvailableTokens()
   */
  @Override
  public List<ArcToken> getAvailableTokens ()
  {
    if ( availableTokens == null )
    {
      availableTokens = new ArrayList<ArcToken>();

      GraphProcess process = getInitiatingToken().getProcess();
      Node targetNode = getInitiatingToken().getArc().getEndNode();

      for ( ArcToken arcToken : process.getActiveArcTokens() )
      {
        if ( arcToken.getArc().getEndNode().equals( targetNode ) )
        {
          availableTokens.add( arcToken );
        }
      }
    }

    return availableTokens;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#getInitiatingToken()
   */
  @Override
  public ArcToken getInitiatingToken ()
  {
    return initiatingToken;
  }

  /**
   * @see com.googlecode.sarasvati.rubric.env.PredicateEnv#evalPredicate(java.lang.String)
   */
  @Override
  public boolean evalPredicate (final String predicate)
  {
    return predicateEnv.evalPredicate( predicate );
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#setApplicable(boolean)
   */
  @Override
  public void setApplicable (final boolean isApplicable)
  {
    this.isApplicable = isApplicable;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#markArcRequired(com.googlecode.sarasvati.ArcToken)
   */
  @Override
  public void markArcRequired (final ArcToken token)
  {
    if ( token.equals(  initiatingToken ) )
    {
      if ( isApplicable )
      {
        isRequired = true;
      }
      else
      {
        isOptional = true;
      }
    }
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#isInitiatingTokenOptional()
   */
  @Override
  public boolean isInitiatingTokenOptional ()
  {
    return isOptional;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#isInitiatingTokenRequired()
   */
  @Override
  public boolean isInitiatingTokenRequired ()
  {
    return isRequired;
  }
}