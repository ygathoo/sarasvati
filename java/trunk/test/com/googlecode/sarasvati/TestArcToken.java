
package com.googlecode.sarasvati;

import junit.framework.Assert;

public class TestArcToken extends TestToken<ArcToken>
{
  protected TestNodeToken parent;
  protected TestNodeToken childToken;
  protected Node          childNode;

  protected boolean       pending;

  public TestArcToken (int lineNumber, TestNodeToken parent, boolean pending, boolean complete, ExecutionType executionType)
  {
    super( lineNumber, complete, executionType );
    this.parent = parent;
    this.pending = pending;
  }

  public TestNodeToken getParent ()
  {
    return parent;
  }

  public void setParent (TestNodeToken parent)
  {
    this.parent = parent;
  }

  public TestNodeToken getChildToken ()
  {
    return childToken;
  }

  public void setChildToken (TestNodeToken childToken)
  {
    this.childToken = childToken;
  }

  public Node getChildNode ()
  {
    return childNode;
  }

  public void setChildNode (Node childNode)
  {
    this.childNode = childNode;
  }

  public boolean isPending ()
  {
    return pending;
  }

  public void setPending (boolean pending)
  {
    this.pending = pending;
  }

  @Override
  public void validate ()
  {
    Assert.assertEquals( "Parent token does not match on " + toString(), parent.getToken(), token.getParentToken() );
    Assert.assertEquals( "IsPending? does not match on " + toString(), pending, token.isPending() );

    if ( isComplete() )
    {
      Assert.assertNotNull( "Completed test arc token should have child token: " + toString(), childToken );
    }
    else
    {
      Assert.assertNotNull( "Incomplete test arc token should have child node: " + toString(), childNode );
      Assert.assertEquals( "Child node does not match on " + toString(), childNode, token.getArc().getEndNode() );
    }

    super.validate();
  }

  @Override
  public String toString ()
  {
    return "[TestArcToken parentId=" + getParent().getId() + " line=" + lineNumber + "]";
  }
}