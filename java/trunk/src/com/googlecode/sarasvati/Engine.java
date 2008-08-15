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
 * An {@link Engine} executes a process. A {@link Graph} specifies
 * how it should be executed and a {@link Process} tracks the current state
 * of execution. But it is an Engine which create instances of {@link ArcToken},
 * {@link NodeToken} and {@link Process} and which invokes
 * {@link Node#guard(Engine, NodeToken)} and {@link Node#execute(Engine, NodeToken)}.
 *
 * @author Paul Lorenz
 */
public interface Engine
{
  /**
   * Given a {@link Graph}, creates a new {@link Process} executing that graph.
   * A {@link NodeToken} will be generated on each start nodes (determined by
   * {@link Graph#getStartNodes()}), and these NodeTokens will be executed.
   * If the graph does not contain Nodes which go into a wait state, the
   * {@link Process} returned will be completed.
   *
   * @param graph The {@link Graph} to execute.
   * @return A {@link Process} executing the given {@link Graph}.
   */
  Process startProcess (Graph graph);

  /**
   * Sometimes it is desirable to separate process creation from
   * starting execution of the process. For example, one may wish
   * to set some variables into the process environment before
   * starting execution.
   *
   * startProcess will generate a new {@link NodeToken} on each
   * start node contained in the given process.
   *
   * @param process The process on which to begin execution.
   */
  void startProcess (Process process);

  /**
   * Competes and cancels may be performed synchronously or asynchronously. If
   * synchronous, {@link #finalizeComplete(Process)} and {@link #finalizeCancel(Process)}
   * will be called directly by the engine. If asynchronous, {@link #doAsyncComplete(Process)}
   * and {@link #doAsyncCancel(Process)} will be called. These methods should be overridden
   * by subclasses. The subclass with then be responsible for calling the appropriate
   * finalize method when ready.
   * <br/>
   * If this method returns false, the process will never enter the {@link ProcessState#PendingCancel}
   * and {@link ProcessState#PendingCompletion} states.
   * <br/>
   *
   * Returns false by default.
   *
   * @return True if complete and cancel should be performed asynchronously.
   */
  boolean isProcessEndAsynchronous ();

  /**
   * Cancels the given process. The process state is set to {@link ProcessState#PendingCancel}.
   *
   * @param process The process to cancel
   */
  void cancelProcess (Process process);

  /**
   * Hook for subclasses to set up asynchronous completion if desired. The process will
   * be in state {@link ProcessState#PendingCompletion}.
  * <br/>
   * Only invoked if {@link #isProcessEndAsynchronous()} returns true;

   * @param process The process being completed.
   */
  void doAsyncComplete (Process process);

  /**
   * Hook for subclasses to set up asynchronous cancellation if desired. The process will
   * be in state {@link ProcessState#PendingCancel}.
   * <br/>
   * Only invoked if {@link #isProcessEndAsynchronous()} returns true;
   *
   * @param process The process being canceled.
   */
  void doAsyncCancel (Process process);

  /**
   * Will set the state to {@link ProcessState#Completed} and perform whatever
   * cleanup is required.
   *
   * If this process is a nested process, at this point the containing
   * token will be completed.
   *
   * @param process The process being completed.
   */
  void finalizeComplete (Process process);

  /**
   * Will set the state to {@link ProcessState#Canceled} and perform whatever
   * cleanup is required.
   *
   * @param process The process being canceled.
   */
  void finalizeCancel (Process process);

  /**
   * Continues execution of a process in a wait state.
   * If a call to {@link Node#execute(Engine, NodeToken)} does not contain a
   * call to {@link Engine#completeExecution(NodeToken, String)}, then execution
   * of the graph will halt at that point. This is generally referred to as a wait
   * state. It may happen, for example, if the action represented by that node
   * must be done by a human or some external system.
   *
   * <br/>
   *
   * When the external system has determined that the {@link Node} has completed its
   * work, it should invoke this method to continue executing the process.
   *
   * <br/>
   *
   * If the token belongs to a process which is _not_ in state {@link ProcessState#Executing}
   * this call will return immediately.
   *
   * @param token   The {@link NodeToken} to resume execution on
   * @param arcName The name of the {@link Arc} (or arcs, as more than one {@link Arc} can
   *                have the same name) to generate ArcTokens on.
   *
   */
  void completeExecution (NodeToken token, String arcName);

  /**
   * Returns an appropriate {@link GraphRepository} for this {@link Engine}. Subclases
   * may override this to provide custom behavior.
   *
   * @return An appropriate {@link GraphRepository} for this {@link Engine}
   */
  GraphRepository<? extends Graph> getRepository ();

  /**
   * Returns an appropriate {@link GraphFactory} for this {@link Engine}. Subclasses
   * may override this provide customer behavior.
   *
   * @return A {@link GraphFactory} which will generate the appropriate types for this {@link Engine}.
   */
  GraphFactory<? extends Graph> getFactory ();
}