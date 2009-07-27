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

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( externalEnd == null )
        ? 0 : externalEnd.hashCode() );
    result = prime * result + ( ( externalStart == null )
        ? 0 : externalStart.hashCode() );
    result = prime * result + ( ( label == null )
        ? 0 : label.hashCode() );
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if ( this == obj ) return true;
    if ( obj == null ) return false;
    if ( !( obj instanceof ArcState ) ) return false;
    ArcState other = (ArcState)obj;
    if ( externalEnd == null )
    {
      if ( other.externalEnd != null ) return false;
    }
    else if ( !externalEnd.equals( other.externalEnd ) ) return false;
    if ( externalStart == null )
    {
      if ( other.externalStart != null ) return false;
    }
    else if ( !externalStart.equals( other.externalStart ) ) return false;
    if ( label == null )
    {
      if ( other.label != null ) return false;
    }
    else if ( !label.equals( other.label ) ) return false;
    return true;
  }
}
