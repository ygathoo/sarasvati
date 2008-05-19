/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

/**
 * The set of tokens in a process represent the current state
 * of the workflow. There are two types of tokens, node tokens
 * and arc tokens. Node tokens point at nodes and arc tokens
 * point to arcs. Tokens are never moved. They may be marked
 * as complete and new tokens will be created in the areas of
 * the workflow now in progress.
 *
 * @author Paul Lorenz
 */
public interface Token
{
  void markComplete ();
}
