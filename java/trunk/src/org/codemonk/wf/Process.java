/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.List;

public interface Process
{
  Graph getGraph ();
  List<? extends ArcToken> getArcTokens ();
  void addArcToken (ArcToken token);
  void removeArcToken (ArcToken token);
  void addNodeToken (NodeToken token);
  void removeNodeToken (NodeToken token);

}
