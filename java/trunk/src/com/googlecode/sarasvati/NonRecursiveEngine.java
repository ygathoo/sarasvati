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
package com.googlecode.sarasvati;

import java.util.LinkedList;

/**
 * {@link Engine} is recursive by default. So if you have a very deep graph
 * or a one which loops without waiting, it is possible to run out of
 * stack space. This implementation turns the recursion into iterative
 * loop, which can handle arbitrarily deep graphs.
 * 
 * Unlike the the default {@link Engine}, this implementation is no longer
 * thread safe. 
 *
 * @author Paul Lorenz
 */
public abstract class NonRecursiveEngine extends Engine
{
  private static class TokenArcNamePair
  {
    public NodeToken token;
    public String    arcName;

    public TokenArcNamePair (NodeToken token, String arcName)
    {
      this.token = token;
      this.arcName = arcName;
    }
  }

  protected LinkedList<TokenArcNamePair> queue = new LinkedList<TokenArcNamePair>();
  protected boolean firstExecution = true;

  @Override
  public void completeExecuteNode (NodeToken token, String arcName)
  {
    if ( !firstExecution )
    {
      queue.add( new TokenArcNamePair( token, arcName ) );
    }
    else
    {
      firstExecution = false;
      super.completeExecuteNode( token, arcName );

      while ( !queue.isEmpty() )
      {
        TokenArcNamePair pair = queue.removeFirst();
        super.completeExecuteNode( pair.token, pair.arcName );
      }
      firstExecution = true;
    }
  }
}