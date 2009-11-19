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
package com.googlecode.sarasvati.join;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.JoinResult;
import com.googlecode.sarasvati.JoinStrategy;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;

/**
 * Implements a join strategy where a node will complete a join
 * whenever any arc token arrives.
 *
 * @author Paul Lorenz
 */
public class MergeJoinStrategy implements JoinStrategy
{
  protected static final Comparator<NodeToken> TOKEN_COMPARATOR =
    new Comparator<NodeToken>()
    {
      @Override
      public int compare (final NodeToken o1, final NodeToken o2)
      {
        return o2.getCreateDate().compareTo( o1.getCreateDate() );
      }
    };


  @Override
  public JoinResult performJoin (final Engine engine, final ArcToken token)
  {
    Node targetNode = token.getArc().getEndNode();
    Collection<NodeToken> nodeTokens = token.getProcess().getTokensOnNode( targetNode, engine );

    if ( nodeTokens.isEmpty() )
    {
      return JoinResult.INCOMPLETE_JOIN_RESULT;
    }

    NodeToken newestToken = Collections.max( nodeTokens, TOKEN_COMPARATOR );

    if ( newestToken.getExecutionType().isBacktracked() )
    {
      return JoinResult.INCOMPLETE_JOIN_RESULT;
    }

    return new CompleteJoinResult( Collections.singletonList( token ) );
  }
}
