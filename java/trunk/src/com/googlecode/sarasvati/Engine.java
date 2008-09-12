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

import com.googlecode.sarasvati.event.ExecutionEvent;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.event.ExecutionListener;
import com.googlecode.sarasvati.load.GraphFactory;
import com.googlecode.sarasvati.load.GraphRepository;


/**
 * An {@link Engine} executes a process. A {@link Graph} specifies
 * how it should be executed and a {@link GraphProcess} tracks the current state
 * of execution. But it is an Engine which creates instances of {@link ArcToken},
 * {@link NodeToken} and {@link GraphProcess} and which invokes
 * {@link Node#guard(Engine, NodeToken)} and {@link Node#execute(Engine, NodeToken)}.
 *
 * @author Paul Lorenz
 */
public interface Engine
{
  /**
   * Given a {@link Graph}, creates a new {@link GraphProcess} executing that graph.
   * A {@link NodeToken} will be generated on each start nodes (determined by
   * {@link Graph#getStartNodes()}), and these NodeTokens will be executed.
   * If the graph does not contain Nodes which go into a wait state, the
   * {@link GraphProcess} returned will be completed.
   *
   * @param graph The {@link Graph} to execute.
   * @return A {@link GraphProcess} executing the given {@link Graph}.
   */
  GraphProcess startProcess (Graph graph);

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
  void startProcess (GraphProcess process);

  /**
   * Cancels the given process. The process state is set to {@link ProcessState#PendingCancel}.
   *
   * @param process The process to cancel
   */
  void cancelProcess (GraphProcess process);

  /**
   * Will set the state to {@link ProcessState#Completed} and perform whatever
   * cleanup is required.
   *
   * If this process is a nested process, at this point the containing
   * token will be completed.
   *
   * @param process The process being completed.
   */
  void finalizeComplete (GraphProcess process);

  /**
   * Will set the state to {@link ProcessState#Canceled} and perform whatever
   * cleanup is required.
   *
   * @param process The process being canceled.
   */
  void finalizeCancel (GraphProcess process);

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
   * Marks the given node completed and generates the next set of arc tokens.
   * However, these arc tokens will not be processed. Execution may be
   * continued later with a call to {@link Engine#executeQueuedArcTokens(GraphProcess)}.
   *
   * @param token   The token to mark completed
   * @param arcName The name of the {@link Arc} (or arcs, as more than one {@link Arc} can
   *                have the same name) to generate ArcTokens on.
   */
  void completeAsynchronous (NodeToken token, String arcName );

  /**
   * If this process has any {@link ArcToken}s queued for execution, this method
   * will execute them.
   *
   * @param process The process whose queued arc tokens to queue
   */
  void executeQueuedArcTokens (GraphProcess process);

  /**
   * Returns an appropriate {@link GraphRepository} for this {@link Engine}. Subclasses
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

  /**
   * This will send the given event to listeners who have registered for
   * events on all processes and to listeners who have registered for events
   * on the process that originated this event.
   *
   * @param event The event to send to all interested listeners.
   */
  void fireEvent (ExecutionEvent event);

  /**
   * Adds a listener for the given event types for all processes. It is not added to
   * each process individually, but rather added to a global set of listeners. Global
   * generally means global for instances of a particular engine types.
   *
   * <br/>
   *
   * {@link ExecutionListener} instances may be be reused and they may be serialized
   * in some fashion. Therefore, they must be thread safe and they must have a
   * default constructor.
   *
   * @param listener The listener
   * @param eventTypes The event types to be notified for.
   */
  void addExecutionListener (ExecutionListener listener, ExecutionEventType...eventTypes);

  /**
   * Adds a listener for the given event types for the given process.
   *
   * <br/>
   *
   * {@link ExecutionListener} instances may be be reused and they may be serialized
   * in some fashion. Therefore, they must be thread safe and they must have a
   * default constructor.
   *
   *
   * @param process The process to add the listener for, or null for all processes
   * @param listener The listener
   * @param eventTypes The event types to be notified for.
   */
  void addExecutionListener (GraphProcess process, ExecutionListener listener, ExecutionEventType...eventTypes);

  /**
   * Will remove the given listener from the set of global listeners.If no event types are specified,
   * the listener will be removed for all event types. Otherwise it will be removed for only the
   * specified event types.
   *
   * <br/>
   * The listener doesn't need to match exactly. All listeners of this type will be matched. What matches
   * types is determined by the implementation, but usually it means same class.
   *
   *
   * @param listener The type of listener to remove
   * @param eventTypes The set of event types to remove the listener for, or none to remove for all
   */
  void removeExecutionListener (ExecutionListener listener, ExecutionEventType...eventTypes);

  /**
   * Will remove the listener from the given proces. If no event types are specified, the listener
   * will be removed for all event types. Otherwise it will be removed for only the specified event types.
   *
   * <br/>
   * The listener doesn't need to match exactly. All listeners of this type will be matched. What matches
   * types is determined by the implementation, but usually it means same class.
   *
   *
   * @param process The process to remove the listener from, or null to remove from the global listener set
   * @param listener The type of listener to remove
   * @param eventTypes The set of event types to remove the listener for, or none to remove for all
   */
  void removeExecutionListener (GraphProcess process, ExecutionListener listener, ExecutionEventType...eventTypes);

  /**
   * Engine implementations can cache instances of {@link ExecutionListener}.
   *
   * @param type The type of the execution listener. Probably a class name, but different implementations of
   *             {@link Engine} may use a different scheme.
   * @return The instance of {@link ExecutionListener} appropriate for this type
   * @throws WorkflowException If an instance for the type cannot be found or created.
   */
  ExecutionListener getExecutionListenerInstance (String type) throws WorkflowException;
}