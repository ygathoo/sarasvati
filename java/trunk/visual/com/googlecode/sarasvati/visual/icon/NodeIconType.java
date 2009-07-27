package com.googlecode.sarasvati.visual.icon;

public enum NodeIconType
{
  Oval("Oval"),
  Rectangular("Rectangular"),
  SmallCircle("Small circle");

  private final String description;

  private NodeIconType (final String description)
  {
    this.description = description;
  }

  @Override
  public String toString ()
  {
    return description;
  }
}
