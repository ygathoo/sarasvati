package com.googlecode.sarasvati;

import java.util.LinkedList;
import java.util.List;

public class TestNodeToken
{
  protected List<TestArcToken> parents  = new LinkedList<TestArcToken>();
  protected List<TestArcToken> children = new LinkedList<TestArcToken>();

  protected Node               node;
  protected boolean            complete;
  protected ExecutionType      executionType;

  public TestNodeToken (Node node, boolean complete, ExecutionType executionType)
  {
    this.node = node;
    this.complete = complete;
    this.executionType = executionType;
  }

  public Node getNode ()
  {
    return node;
  }

  public void setNode (Node node)
  {
    this.node = node;
  }

  public boolean isComplete ()
  {
    return complete;
  }

  public void setComplete (boolean complete)
  {
    this.complete = complete;
  }

  public List<TestArcToken> getParents ()
  {
    return parents;
  }

  public void setParents (List<TestArcToken> parents)
  {
    this.parents = parents;
  }

  public List<TestArcToken> getChildren ()
  {
    return children;
  }

  public void setChildren (List<TestArcToken> children)
  {
    this.children = children;
  }

  public void addChild (TestArcToken token)
  {
    children.add( token );
  }

  public void addParent (TestArcToken token)
  {
    parents.add( token );
  }
}