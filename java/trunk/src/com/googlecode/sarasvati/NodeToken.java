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

/**
 * Node tokens point to nodes in the graph. Unlike arc tokens,
 * they may have attributes associated with them.
 *
 * @author Paul Lorenz
 */
public interface NodeToken extends Token
{
  /**
   * Returns the node that this token points to.
   *
   * @return The node associated node.
   */
  Node getNode ();

  /**
   * Returns the process that this node token belongs to.
   *
   * @return The associated process
   */
  Process getProcess ();

  /**
   * Returns the full environment. This will include variables set on the
   * token as well as variables set on the process. If a variable with the
   * same name exists in both the token and process environments, the token
   * variable will be returned.
   *
   * NOTE: Changes can only be made at the token level. Updates or removals
   *       will only be applied to the token attributes.
   *
   * @return An {@link Env} containing variables from this token and from the {@link Process}
   * @see Process#getEnv()
   */
  Env getFullEnv ();

  /**
   * Returns the environment for just this token. Variables defined on the
   * process will not be found.
   *
   * @return An {@link Env} containing variables from just this token
   */
  Env getEnv ();

  /**
   * A NodeToken is evaluated by the {@link Node#guard(Engine, NodeToken)}
   * method. recordGuardAction will be called by the engine to
   * record the result of the guard.
   *
   * @param engine The {@link Engine} being used to execute the associated {@link Process}.
   * @param action The {@link GuardAction} taken with this NodeToken.
   * @see Node#guard(Engine, NodeToken)
   */
  void recordGuardAction (Engine engine, GuardAction action);

  /**
   * Returns the GuardAction that was returned from the Node guard
   * called on this token or null if the guard has not completed
   * or been called.
   *
   * @return The result of the Node guard called on this token, or
   *         null if the guard has not completed or been called.
   */
  GuardAction getGuardAction ();
}
