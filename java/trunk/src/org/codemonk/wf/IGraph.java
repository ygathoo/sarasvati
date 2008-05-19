/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.List;

public interface IGraph
{
  String getName ();

  List<IArc> getInputArcs (INode node);
  List<IArc> getOutputArcs (INode node);

  List<IArc> getInputArcs (INode node, String arcName);
  List<IArc> getOutputArcs (INode node, String arcName);

  List<INode> getStartNodes ();
}
