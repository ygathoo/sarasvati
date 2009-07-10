package com.googlecode.sarasvati.editor.model;

public class ArcState
{
  private final String externalStart;
  private final String externalEnd;
  private final String label;

  public ArcState (String label, String externalStart, String externalEnd)
  {
    this.externalStart = externalStart;
    this.externalEnd = externalEnd;
    this.label = label;
  }

  public String getExternalStart ()
  {
    return externalStart;
  }

  public String getExternalEnd ()
  {
    return externalEnd;
  }

  public String getLabel ()
  {
    return label;
  }
}
