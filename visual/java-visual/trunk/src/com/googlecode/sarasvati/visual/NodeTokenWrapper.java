/**
 * Created on Oct 8, 2008
 */
package com.googlecode.sarasvati.visual;

import java.util.LinkedList;
import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.NodeToken;

class NodeTokenWrapper
{
  private NodeToken token;
  private List<ArcToken> children;

  public NodeTokenWrapper (NodeToken token)
  {
    this.token = token;
    this.children = new LinkedList<ArcToken>();
  }

  public NodeToken getToken ()
  {
    return token;
  }

  public List<ArcToken> getParents ()
  {
    return token.getParentTokens();
  }

  public List<ArcToken> getChildren ()
  {
    return children;
  }

  public void addChild (ArcToken child)
  {
    children.add( child );
  }
}