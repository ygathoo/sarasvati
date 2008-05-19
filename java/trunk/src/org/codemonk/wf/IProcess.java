/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.List;

public interface IProcess
{
  IGraph getGraph ();
  List<? extends IArcToken> getArcTokens ();
  void addArcToken (IArcToken token);
  void removeArcToken (IArcToken token);
}
