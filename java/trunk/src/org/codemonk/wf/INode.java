/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

public interface INode
{
  String getName ();
  String getType ();
  boolean isJoin ();
  GuardResponse guard (WfRun wfRun, INodeToken token);
  void execute(Engine engine, WfRun wfRun, INodeToken token);
}
