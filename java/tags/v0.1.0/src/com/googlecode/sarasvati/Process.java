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
/**
 * Created on Apr 25, 2008
 */
package com.googlecode.sarasvati;

import java.util.List;

/**
 * A Process represents a currently executing workflow graph.
 *
 * @author Paul Lorenz
 */
public interface Process
{
  /**
   * Returns the graph being executed.
   *
   * @return The graph being executed.
   */
  Graph getGraph ();

  /**
   * Returns the list of current ArcTokens.
   *
   * @return The list of current ArcTokens.
   */
  List<? extends ArcToken> getArcTokens ();

  /**
   * Returns the list of current NodeTokens.
   *
   * @return The list of current NodeTokens.
   */
  List<? extends NodeToken> getNodeTokens ();

  /**
   * Adds an ArcToken to the list of active ArcTokens.
   *
   * @param token The token being added
   */
  void addArcToken (ArcToken token);

  /**
   * Removes an ArcToken from the list of active ArcTokens.
   *
   * @param token The token being removed.
   */
  void removeArcToken (ArcToken token);

  /**
   * Adds a NodeToken to the list of active NodeTokens.
   *
   * @param token The token being added
   */
  void addNodeToken (NodeToken token);

  /**
   * Removes a NodeToken from the list of active NodeTokens.
   *
   * @param token The token being removed.
   */
  void removeNodeToken (NodeToken token);

  /**
   * Return true if the process has completed, false otherwise. A process
   * is complete if it has no active tokens, either ArcTokens or NodeTokens.
   *
   * @return True if the process has completed, false otherwise.
   */
  boolean isComplete ();
}