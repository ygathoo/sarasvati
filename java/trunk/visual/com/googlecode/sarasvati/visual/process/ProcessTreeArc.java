package com.googlecode.sarasvati.visual.process;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;

public class ProcessTreeArc implements VisualProcessArc
{
  protected ArcToken token;
  protected Arc      arc;
  protected ProcessTreeNode parent;
  protected ProcessTreeNode child;

  public ProcessTreeArc(ArcToken token, ProcessTreeNode parent, ProcessTreeNode child)
  {
    this.token  = token;
    this.parent = parent;
    this.child  = child;
  }

  public ProcessTreeArc(Arc arc, ProcessTreeNode parent, ProcessTreeNode child)
  {
    this.arc    = arc;
    this.parent = parent;
    this.child  = child;
  }

  public ArcToken getToken()
  {
    return token;
  }

  public Arc getArc ()
  {
    return token == null ? arc : token.getArc();
  }

  public ProcessTreeNode getParent()
  {
    return parent;
  }

  public ProcessTreeNode getChild()
  {
    return child;
  }
}
