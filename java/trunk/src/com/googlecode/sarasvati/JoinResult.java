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

import java.util.List;

/**
 * Encapsulates the results of a join attempt. Includes whether
 * or not the join is complete. If the join is complete, also
 * includes the list of arc tokens which completed the join.
 * 
 * @author Paul Lorenz
 */
public interface JoinResult
{
  /**
   * Shared instance of an incomplete join result, which always returns
   * false for isJoinComplete.
   */
  public static JoinResult INCOMPLETE_JOIN_RESULT = new JoinResult()
  {
    /**
     * Always returns false.
     * 
     * @see JoinResult#isJoinComplete()
     */
    @Override
    public boolean isJoinComplete ()
    {
      return false;
    }
  
    /**
     * Always throws an {@link IllegalStateException}
     * @see JoinResult#getArcTokensCompletingJoin()
     */
    @Override
    public List<ArcToken> getArcTokensCompletingJoin ()
    {
      throw new IllegalStateException( "getArcTokensCompletingJoin should never be called if isJoinComplete returns false." );
    }
  };
  
  /**
   * Returns true if the join is complete, false otherwise.
   * 
   * @return True if the join is complete, false otherwise.
   */
  boolean isJoinComplete ();
  
  /**
   * Returns the ArcTokens which were required to complete this join, and will be considered
   * the parents of the new {@link NodeToken}. If {@link JoinResult#isJoinComplete()} would
   * return false, this method should throw an {@link IllegalStateException}.
   * 
   * @returns The ArcTokens which were required to complete this join, and will be considered
   *          the parents of the new {@link NodeToken}. 
   * 
   * @throws IllegalStateException If this is invoked when isJoinComplete returns false.
   */
  List<ArcToken> getArcTokensCompletingJoin (); 
}
