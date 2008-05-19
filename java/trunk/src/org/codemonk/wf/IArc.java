/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

public interface IArc
{
  static String DEFAULT_ARC = "";

  INode getStartNode ();
  INode getEndNode ();
  String getName ();
}
