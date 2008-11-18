package com.googlecode.sarasvati.editor.model;

public class EditorNode
{
  protected String  name;
  protected String  type;
  protected boolean isStart;
  protected boolean isJoin;

  public String getName()
  {
    return name;
  }

  public void setName(String name)
  {
    this.name = name;
  }

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
}