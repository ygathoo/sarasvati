/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
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