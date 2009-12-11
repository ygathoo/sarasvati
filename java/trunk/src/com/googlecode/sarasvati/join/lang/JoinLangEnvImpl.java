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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.JoinResult;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.Token;
import com.googlecode.sarasvati.TokenSet;
import com.googlecode.sarasvati.impl.NodeTokenComparator;
import com.googlecode.sarasvati.join.CompleteJoinResult;
import com.googlecode.sarasvati.join.IncompleteJoinResult;
import com.googlecode.sarasvati.join.MergeJoinResult;
import com.googlecode.sarasvati.rubric.env.PredicateEnv;
import com.googlecode.sarasvati.util.SvUtil;

public class JoinLangEnvImpl implements JoinLangEnv
{
  private final Engine engine;
  private final PredicateEnv predicateEnv;
  private final ArcToken initiatingToken;
  private List<ArcToken> availableTokens = null;
  private Set<ArcToken> affectedTokens = new HashSet<ArcToken>();
  private Map<String, TokenSet> tokenSets;

  private boolean mergeTokenInitialized = false;
  private NodeToken mergeToken = null;
  private boolean isApplicable;
  private boolean isOptional;
  private boolean isRequired;

  public JoinLangEnvImpl (final Engine engine, final ArcToken initiatingToken, final PredicateEnv predicateEnv)
  {
    this.engine = engine;
    this.initiatingToken = initiatingToken;
    this.predicateEnv = predicateEnv;
  }


  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#getEngine()
   */
  @Override
  public Engine getEngine ()
  {
    return engine;
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

  @Override
  public boolean isInitiatingTokenIncludedInJoin ()
  {
    return isRequired || isOptional;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangEnv#includeInJoin(com.googlecode.sarasvati.ArcToken)
   */
  @Override
  public void includeInJoin (final ArcToken token)
  {
    affectedTokens.add( token );

    if ( token == initiatingToken )
    {
      if ( isApplicable )
      {
        isRequired =true;
      }
      else
      {
        isOptional = true;
      }
    }
  }

  @Override
  public void includeInJoin (final Collection<ArcToken> tokens)
  {
    for ( ArcToken token : tokens )
    {
      includeInJoin( token );
    }
  }

  private void initializeMergeTokenIfNecesary ()
  {
    if ( !mergeTokenInitialized )
    {
      Node targetNode = initiatingToken.getArc().getEndNode();
      Collection<NodeToken> nodeTokens = initiatingToken.getProcess().getTokensOnNode( targetNode, engine );

      if ( !nodeTokens.isEmpty() )
      {
        NodeToken newestToken = Collections.max( nodeTokens, NodeTokenComparator.INSTANCE );

        if ( !newestToken.getExecutionType().isBacktracked() )
        {
          mergeToken = newestToken;
        }
      }
    }
  }

  public JoinResult mergeIfPossible ()
  {
    initializeMergeTokenIfNecesary();

    if ( mergeToken != null )
    {
      return new MergeJoinResult( mergeToken );
    }

    return IncompleteJoinResult.INSTANCE;
  }

  @Override
  public JoinResult finishJoin ()
  {
    if ( affectedTokens.contains( initiatingToken ) )
    {
      return new CompleteJoinResult( new ArrayList<ArcToken>( affectedTokens ) );
    }

    return mergeIfPossible();
  }

  @Override
  public TokenSet getTokenSet (final String tokenSetName)
  {
    if ( tokenSets == null )
    {
      tokenSets = new HashMap<String, TokenSet>();
    }

    if ( tokenSets.containsKey( tokenSetName ) )
    {
      return tokenSets.get( tokenSetName );
    }

    for ( Token token : getAvailableTokens() )
    {
      TokenSet tokenSet = SvUtil.getTokenSet( token, tokenSetName );
      if ( tokenSet != null )
      {
        tokenSets.put( tokenSetName, tokenSet );
        return tokenSet;
      }
    }

    tokenSets.put( tokenSetName, null );
    return null;
  }

  public void reset ()
  {
    this.affectedTokens = new HashSet<ArcToken> ();
    this.isOptional = false;
    this.isRequired = false;
  }
}