/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;


public interface INode
{
  String getName ();
  String getType ();
  boolean isJoin ();
  GuardResponse guard (IProcess process, INodeToken token);
  void execute(Engine engine, IProcess process, INodeToken token);
}
