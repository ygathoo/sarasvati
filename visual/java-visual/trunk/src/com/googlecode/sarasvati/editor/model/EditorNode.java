package com.googlecode.sarasvati.editor.model;

public class EditorNode extends EditorGraphMember
{
  protected String  type;
  protected boolean isStart;
  protected boolean isJoin;
  protected String  guard;

  public String getType()
  {
    return type;
  }

  public void setType(String type)
  {
    this.type = type;
  }

  public boolean isStart()
  {
    return isStart;
  }

  public void setStart(boolean isStart)
  {
    this.isStart = isStart;
  }

  public boolean isJoin()
  {
    return isJoin;
  }

  public void setJoin(boolean isJoin)
  {
    this.isJoin = isJoin;
  }

  public String getGuard()
  {
    return guard;
  }

  public void setGuard( String guard )
  {
    this.guard = guard;
  }
}