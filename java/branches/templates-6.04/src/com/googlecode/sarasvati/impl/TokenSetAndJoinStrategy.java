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
package com.googlecode.sarasvati.impl;

import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.ArcTokenSetMember;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.JoinResult;
import com.googlecode.sarasvati.JoinStrategy;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.TokenSet;

/**
 * Implements a join strategy in which nodes will wait for arc tokens to be
 * present on all incoming arcs before completing the join. If the incoming
 * arc token does not belong to a token set, an {@link IllegalStateException}
 * will be thrown. This behavior could be changed in a subclass by overriding
 * {@link TokenSetAndJoinStrategy#performFallbackJoin(Engine, GraphProcess, ArcToken)}.
 *
 * @author Paul Lorenz
 */
public class TokenSetAndJoinStrategy implements JoinStrategy
{
  public TokenSet getTokenSet (ArcToken token)
  {
    List<ArcTokenSetMember> tokenSetMemberShips = token.getTokenSetMemberships();
    return tokenSetMemberShips.isEmpty() ? null : tokenSetMemberShips.get( 0 ).getTokenSet();
  }

  /**
   * Called if the node token is not a member of a suitable token set.
   * Throws {@link IllegalStateException} by default, but this behavior
   * may be overridden in a subclass.
   *
   * @param engine The engine performing the join
   * @param process The currently executing process.
   * @param token The token being join on.
   *
   * @return
   */
  public JoinResult performFallbackJoin (Engine engine, GraphProcess process, ArcToken token)
  {
    throw new IllegalStateException( "Token " + token + " does not belong to the appropriate token set" );
  }

  @Override
  public JoinResult performJoin (Engine engine, GraphProcess process, ArcToken token)
  {
    TokenSet tokenSet = getTokenSet( token );

    if ( tokenSet == null )
    {
      return performFallbackJoin( engine, process, token );
    }

    if ( !tokenSet.getActiveNodeTokens().isEmpty() )
    {
      return JoinResult.INCOMPLETE_JOIN_RESULT;
    }

    Node targetNode = token.getArc().getEndNode();
    List<ArcToken> activeMembers = tokenSet.getActiveArcTokens();

    for ( ArcToken setMember : activeMembers )
    {
      if ( !setMember.getArc().getEndNode().equals( targetNode ) )
      {
        return JoinResult.INCOMPLETE_JOIN_RESULT;
      }
    }

    tokenSet.markComplete( engine );

    return new CompleteJoinResult( activeMembers );
  }
}