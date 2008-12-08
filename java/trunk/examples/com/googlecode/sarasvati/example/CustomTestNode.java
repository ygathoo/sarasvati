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

package com.googlecode.sarasvati.example;

import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeToken;

public class CustomTestNode extends CustomNode
{
  public static class CustomInner
  {
    protected String valueType;
    protected String value;
    protected String test;

    public String getValueType ()
    {
      return valueType;
    }

    public void setValueType (String valueType)
    {
      this.valueType = valueType;
    }

    public String getValue ()
    {
      return value;
    }

    public void setValue (String value)
    {
      this.value = value;
    }

    public String getTest ()
    {
      return test;
    }

    public void setTest (String test)
    {
      this.test = test;
    }
  }

  protected int    size;
  protected String label;
  protected CustomInner inner;

  public CustomTestNode ()
  {
    this.inner = new CustomInner();
  }

  public int getSize ()
  {
    return size;
  }

  public void setSize (int size)
  {
    this.size = size;
  }

  public String getLabel ()
  {
    return label;
  }

  public void setLabel (String label)
  {
    this.label = label;
  }

  public CustomInner getInner ()
  {
    return inner;
  }

  public void setInner (CustomInner inner)
  {
    this.inner = inner;
  }

  @Override
  public void execute (Engine engine, NodeToken token)
  {
    System.out.println( "Size: " + size );
    System.out.println( "Label: " + label );
  }
}