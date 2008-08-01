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

import java.util.HashMap;
import java.util.Map;

/**
 * Implements {@link Env} using a Map
 *
 * @author Paul Lorenz
 */
public class MapEnv implements Env
{
  protected Map<String,String> attributes;
  protected Map<String,Object> transientAttributes;

  /**
   * Create a MapEnv which uses a {@link HashMap} for storage.
   */
  public MapEnv ()
  {
    this.attributes = new HashMap<String, String>();
    this.transientAttributes = new HashMap<String,Object>();
  }

  public MapEnv (Map<String,String> attributes)
  {
    this.attributes = attributes;
  }

  @Override
  public boolean getBooleanAttribute (String name)
  {
    return Boolean.valueOf( attributes.get( name ) );
  }

  @Override
  public long getLongAttribute (String name)
  {
    String val = attributes.get( name );

    if ( val == null )
    {
      return 0;
    }

    try
    {
      return Long.parseLong( val );
    }
    catch (NumberFormatException nfe)
    {
      return 0;
    }
  }

  @Override
  public String getStringAttribute (String name)
  {
    return attributes.get( name );
  }

  @Override
  public boolean hasAttribute (String name)
  {
    return attributes.containsKey( name );
  }

  @Override
  public void removeAttribute (String name)
  {
    attributes.remove( name );
  }

  @Override
  public void setBooleanAttribute (String name, boolean value)
  {
    attributes.put( name, String.valueOf( value ) );
  }

  @Override
  public void setLongAttribute (String name, long value)
  {
    attributes.put( name, String.valueOf( value ) );
  }

  @Override
  public void setStringAttribute (String name, String value)
  {
    attributes.put( name, value );
  }

  @Override
  public Iterable<String> getAttributeNames ()
  {
    return attributes.keySet();
  }

  @Override
  public void setTransientAttribute (String name, Object value)
  {
    transientAttributes.put( name, value );
  }

  @Override
  public boolean hasTransientAttribute (String name)
  {
    return transientAttributes.containsKey( name );
  }

  @Override
  public Object getTransientAttribute (String name)
  {
    return transientAttributes.get( name );
  }

  @Override
  public void removeTransientAttribute (String name)
  {
    transientAttributes.remove( name );
  }
}