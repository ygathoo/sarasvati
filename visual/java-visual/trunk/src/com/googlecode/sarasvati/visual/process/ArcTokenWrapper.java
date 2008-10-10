package com.googlecode.sarasvati.visual.process;

import com.googlecode.sarasvati.ArcToken;

public class ArcTokenWrapper
{
  protected ArcToken token;
  protected NodeTokenWrapper parent;
  protected NodeTokenWrapper child;

  public ArcTokenWrapper(ArcToken token, NodeTokenWrapper parent, NodeTokenWrapper child)
  {
    super();
    this.token = token;
    this.parent = parent;
    this.child = child;
  }

  public ArcToken getToken()
  {
    return token;
  }

  public NodeTokenWrapper getParent()
  {
    return parent;
  }

  public NodeTokenWrapper getChild()
  {
    return child;
  }
}
