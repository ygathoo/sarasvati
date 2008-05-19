/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;


public interface Node
{
  String getName ();
  String getType ();
  boolean isJoin ();
  GuardResponse guard (Process process, NodeToken token);
  void execute(Engine engine, Process process, NodeToken token);
}
