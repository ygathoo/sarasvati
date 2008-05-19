/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

public interface Arc
{
  static String DEFAULT_ARC = "";

  Node getStartNode ();
  Node getEndNode ();
  String getName ();
}
