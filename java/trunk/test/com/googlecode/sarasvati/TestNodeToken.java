package com.googlecode.sarasvati;

import java.util.LinkedList;
import java.util.List;

import junit.framework.Assert;

public class TestNodeToken extends TestToken<NodeToken>
{
  protected List<TestArcToken> parents  = new LinkedList<TestArcToken>();
  protected List<TestArcToken> children = new LinkedList<TestArcToken>();

  protected String             id;
  protected Node               node;

  public TestNodeToken (int lineNumber, String id, Node node, boolean complete, ExecutionType executionType)
  {
    super( lineNumber, complete, executionType );
    this.lineNumber = lineNumber;
    this.id = id;
    this.node = node;
  }

  public String getId ()
  {
    return id;
  }

  public void setId (String id)
  {
    this.id = id;
  }

  public Node getNode ()
  {
    return node;
  }

  public void setNode (Node node)
  {
    this.node = node;
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

  public void addChild (TestArcToken childToken)
  {
    children.add( childToken );
  }

  public void addParent (TestArcToken parentToken)
  {
    parents.add( parentToken );
  }

  @Override
  public void validate ()
  {
    Assert.assertEquals( "Node does not match on " + toString(), node, token.getNode() );
    super.validate();
  }

  @Override
  public String toString ()
  {
    return "[TestNodeToken id=" + id + " line=" + lineNumber + "]";
  }
}