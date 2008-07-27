/**
 * Created on Jun 26, 2008
 */
package com.googlecode.sarasvati;

import java.util.List;

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
   * Cancels the given process. The process state is set to {@link ProcessState#PendingCancel}.
   *
   * @param process The process to cancel
   */
  void cancelProcess (Process process);

  /**
   * Generates a new {@link NodeToken} for the given {@link Process}, pointing
   * to the given {@link Node}.
   *
   * @param process The {@link Process} which the new {@link NodeToken} will belong to
   * @param node    The {@link Node} the new {@link NodeToken} is being placed on
   * @param parents The list of ArcTokens which preceded this {@link NodeToken}
   * @return        A new {@link NodeToken}
   */
  NodeToken newNodeToken (Process process, Node node, List<ArcToken> parents);

  /**
   * Generates a new {@link ArcToken} for the given {@link Process}, pointing
   * to the given {@link Arc}.
   *
   * @param process The {@link Process} which the new ArcToken will belong to
   * @param arc     The {@link Arc} the new ArcToken is being placed on
   * @param parent  The {@link NodeToken} which preceded this {@link ArcToken}.
   * @return A new {@link ArcToken}
   */
  ArcToken newArcToken (Process process, Arc arc, NodeToken parent);

  /**
   * Generates a new {@link Process} for the given {@link Graph}. Execution
   * of the process is not started by this method.
   *
   * The method will be used by {@link Engine#startProcess(Graph)}, and
   * should not need to be called otherwise.
   *
   * @param graph The {@link Graph} the Process will be executing.
   * @return A new {@link Process}
   */
  Process newProcess (Graph graph);

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
}