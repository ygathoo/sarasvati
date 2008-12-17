package com.googlecode.sarasvati;

public class TestArcToken
{
  protected TestNodeToken parent;
  protected TestNodeToken child;

  protected boolean pending;
  protected boolean complete;
  protected ExecutionType executionType;
  protected String parentId;

  public TestArcToken (TestNodeToken parent, boolean pending, boolean complete, ExecutionType executionType)
  {
    this.parent = parent;
    this.pending = pending;
    this.complete = complete;
    this.executionType = executionType;
  }

  public TestNodeToken getParent ()
  {
    return parent;
  }

  public void setParent (TestNodeToken parent)
  {
    this.parent = parent;
  }

  public TestNodeToken getChild ()
  {
    return child;
  }

  public void setChild (TestNodeToken child)
  {
    this.child = child;
  }

  public boolean isPending ()
  {
    return pending;
  }

  public void setPending (boolean pending)
  {
    this.pending = pending;
  }

  public boolean isComplete ()
  {
    return complete;
  }

  public void setComplete (boolean complete)
  {
    this.complete = complete;
  }

  public ExecutionType getExecutionType ()
  {
    return executionType;
  }

  public void setExecutionType (ExecutionType executionType)
  {
    this.executionType = executionType;
  }
}
