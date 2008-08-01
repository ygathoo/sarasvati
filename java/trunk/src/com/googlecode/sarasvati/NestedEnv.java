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

import java.util.HashSet;
import java.util.Set;

public class NestedEnv implements Env
{
  protected Env outerEnv;
  protected Env innerEnv;

  public NestedEnv (Env outerEnv, Env innerEnv)
  {
    this.outerEnv = outerEnv;
    this.innerEnv = innerEnv;
  }

  @Override
  public Iterable<String> getAttributeNames()
  {
    Set<String> names = new HashSet<String>();

    for ( String name : outerEnv.getAttributeNames() )
    {
      names.add( name );
    }

    for ( String name : innerEnv.getAttributeNames() )
    {
      names.add( name );
    }

    return names;
  }

  @Override
  public boolean getBooleanAttribute(String name)
  {
    return outerEnv.hasAttribute( name ) ? outerEnv.getBooleanAttribute( name ) :
                                           innerEnv.getBooleanAttribute( name );
  }

  @Override
  public long getLongAttribute(String name)
  {
    return outerEnv.hasAttribute( name ) ? outerEnv.getLongAttribute( name ) :
                                           innerEnv.getLongAttribute( name );
  }

  @Override
  public String getStringAttribute(String name)
  {
    return outerEnv.hasAttribute( name ) ? outerEnv.getStringAttribute( name ) :
                                           innerEnv.getStringAttribute( name );
  }

  @Override
  public boolean hasAttribute(String name)
  {
    return outerEnv.hasAttribute( name ) || innerEnv.hasAttribute( name );
  }

  @Override
  public void removeAttribute(String name)
  {
    outerEnv.removeAttribute( name );
  }

  @Override
  public void setBooleanAttribute(String name, boolean value)
  {
    outerEnv.setBooleanAttribute(name, value);
  }

  @Override
  public void setLongAttribute(String name, long value)
  {
    outerEnv.setLongAttribute(name, value);
  }

  @Override
  public void setStringAttribute(String name, String value)
  {
    outerEnv.setStringAttribute(name, value);
  }

  @Override
  public void setTransientAttribute (String name, Object value)
  {
    outerEnv.setTransientAttribute( name, value );
  }

  @Override
  public boolean hasTransientAttribute (String name)
  {
    return outerEnv.hasAttribute( name ) || innerEnv.hasAttribute( name );
  }

  @Override
  public Object getTransientAttribute (String name)
  {
    return outerEnv.hasAttribute( name ) ? outerEnv.getTransientAttribute( name ) : innerEnv.getTransientAttribute( name );
  }

  @Override
  public void removeTransientAttribute (String name)
  {
    outerEnv.removeTransientAttribute( name );
  }
}