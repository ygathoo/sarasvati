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

import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.rubric.env.PredicateEnv;

public interface JoinLangEnv extends PredicateEnv
{
  ArcToken getInitiatingToken();
  List<ArcToken> getAvailableTokens ();
  void markArcRequired (ArcToken token);
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
}
