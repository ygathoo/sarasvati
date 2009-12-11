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

import java.util.Collection;
import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.JoinResult;
import com.googlecode.sarasvati.TokenSet;
import com.googlecode.sarasvati.rubric.env.PredicateEnv;

public interface JoinLangEnv extends PredicateEnv
{
  /**
   * Returns the engine instance performing the join
   *
   * @return The engine instance performing the join
   */
  Engine getEngine ();

  /**
   * Returns the arc token which initiated this join operation.
   *
   * @return the arc token which initiated this join operation.
   */
  ArcToken getInitiatingToken();

  /**
   * Returns the arc tokens which are present at the node being joined. Other
   * arc tokens may be involved in the join. These however are those most
   * likely to be used, and are therefore easily accessible and should be
   * cached for performance reasons.
   *
   * @return the arc tokens which are present at the node being joined.
   */
  List<ArcToken> getAvailableTokens ();

  /**
   * Indicates that the given token should be included in the given join operations.
   *
   * @param token The token to be included.
   */
  void includeInJoin (ArcToken token);

  /**
   * Indicates that the given tokens should be included in the given join operations.
   *
   * @param token The tokens to be included.
   */
  void includeInJoin (Collection<ArcToken> tokens);

  /**
   * Indicates if the requirement being evaluated is applicable (does not have
   * a 'when' clause which evaluated to false).
   *
   * @param isApplicable True if the requirement being evaluated is applicable.
   */
  void setApplicable (boolean isApplicable);

  /**
   * Returns true if the initiating token is covered
   * by a require statements that is in effect. A require
   * statement is in effect if it has no 'when' clause
   * or if the 'when' clause evaluates to true.
   *
   * @return true if the initiating token is covered
   * by a require statements that is in effect, and false otherwise.
   */
  boolean isInitiatingTokenRequired ();

  /**
   * Returns true if the initiating token is covered
   * by a require statements that is not in effect. A require
   * statement is in effect if it has no 'when' clause
   * or if the 'when' clause evaluates to true.
   *
   * @return true if the initiating token is covered
   * by a require statements that is not in effect,
   * and false otherwise.
   */
  boolean isInitiatingTokenOptional ();

  /**
   * Returns true if the initiating token was referenced in some
   * way (is either required or optional), and false otherwise.
   * @return true if the initiating token was referenced in some way
   */
  boolean isInitiatingTokenIncludedInJoin ();

  /**
   * Assuming that the all requirements were met, will return an
   * appropriate join result.
   *
   * @return The join result
   */
  JoinResult finishJoin ();

  /**
   * Will attempt to merge the initiating token into a previous join result. If there
   * is no valid node token to merge the arc token into, an incomplete join result
   * will be returned.
   *
   * @return A JoinResult of with a merge action if possible, otherwise an incomplete join action.
   */
  JoinResult mergeIfPossible ();

  /**
   * Looks for a token set with the given name on
   * the tokens which are arriving at the joining node.
   * If none is found, returns null.
   *
   * @param tokenSetName the token set name
   *
   * @return The token set, or null if no token set with that name is found.
   */
  TokenSet getTokenSet (String tokenSetName);

  /**
   * Clears all fields so the environment can be reused.
   */
  void reset ();
}
