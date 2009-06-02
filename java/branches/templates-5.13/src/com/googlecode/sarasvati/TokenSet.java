/**
 * Created on Apr 27, 2009
 */
package com.googlecode.sarasvati;

import java.util.List;

public interface TokenSet
{
  GraphProcess getProcess ();
  String getName ();

  List<ArcToken> getActiveArcTokens ();
  List<NodeToken> getActiveNodeTokens ();

  void addArcToken (ArcToken token, int index);
  void addNodeToken (NodeToken token, int index);
}
