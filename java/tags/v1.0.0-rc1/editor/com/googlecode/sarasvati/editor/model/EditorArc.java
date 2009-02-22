package com.googlecode.sarasvati.editor.model;

public class EditorArc
{
  protected EditorGraphMember start;

  protected EditorGraphMember end;

  protected String            label;

  public EditorGraphMember getStart()
  {
    return start;
  }

  public void setStart( EditorGraphMember start )
  {
    this.start = start;
  }

  public EditorGraphMember getEnd()
  {
    return end;
  }

  public void setEnd( EditorGraphMember end )
  {
    this.end = end;
  }

  public String getLabel()
  {
    return label;
  }

  public void setLabel( String label )
  {
    this.label = label;
  }
}
