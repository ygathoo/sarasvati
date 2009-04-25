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
package com.googlecode.sarasvati.editor.model;

public class EditorArc
{
  protected EditorGraphMember start;
  protected EditorGraphMember end;

  protected String externalStart;
  protected String externalEnd;

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

  public boolean isExternalOutArc ()
  {
    return end.isExternal();
  }

  public boolean isExternalInArc ()
  {
    return start.isExternal();
  }

  public String getExternalStart()
  {
    return externalStart;
  }

  public void setExternalStart( String externalStart )
  {
    this.externalStart = externalStart;
  }

  public String getExternalEnd()
  {
    return externalEnd;
  }

  public void setExternalEnd( String externalEnd )
  {
    this.externalEnd = externalEnd;
  }
}