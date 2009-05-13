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
package com.googlecode.sarasvati.impl;

import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.JoinResult;

/**
 * Represents a complete join result, encapsulating the arc tokens
 * which completed the join.
 *
 * @author Paul Lorenz
 */
public class CompleteJoinResult implements JoinResult
{
  private final List<ArcToken> tokens;

  public CompleteJoinResult (final List<ArcToken> tokens)
  {
    this.tokens = tokens;
  }

  /**
   * @see JoinResult#getArcTokensCompletingJoin()
   */
  @Override
  public List<ArcToken> getArcTokensCompletingJoin ()
  {
    return tokens;
  }

  /**
   * Always returns true.
   *
   * @see JoinResult#isJoinComplete()
   */
  @Override
  public boolean isJoinComplete ()
  {
    return true;
  }
}
