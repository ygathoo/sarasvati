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
/**
 * Created on Apr 25, 2008
 */
package com.googlecode.sarasvati.hib;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;


public class HibTokenSetEnv
{
  protected final HibTokenSet tokenSet;
  protected final Map<String,HibTokenSetProperty>[] props;

  @SuppressWarnings("unchecked")
  public HibTokenSetEnv (final HibTokenSet tokenSet)
  {
    this.tokenSet = tokenSet;
    this.props = new Map[ tokenSet.getMaxMemberIndex() ];

    for ( int i = 0; i< tokenSet.getMaxMemberIndex(); i++ )
    {
      props[i] = new HashMap<String, HibTokenSetProperty>();
    }

    for ( HibTokenSetProperty property : tokenSet.getProperties() )
    {
      props[ property.memberIndex ].put( property.getName(), property );
    }
  }

  public String getStringAttribute (final int memberIndex,
                                    final String name)
  {
    if ( memberIndex < 0 || memberIndex >= props.length )
    {
      return null;
    }

    HibTokenSetProperty property = props[ memberIndex ].get( name );
    return property == null ? null : property.getValue();
  }

  public boolean getBooleanAttribute (final int memberIndex,
                                      final String name)
  {
    return Boolean.valueOf( getStringAttribute( memberIndex, name ) );
  }

  public long getLongAttribute (final int memberIndex,
                                final String name)
  {
    String val = getStringAttribute( memberIndex, name );

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

  public void setStringAttribute (final int memberIndex,
                                  final String name,
                                  final String value)
  {
    if ( memberIndex < 0 || memberIndex >= props.length )
    {
      throw new IllegalArgumentException( "Given memberIndex of " + memberIndex + " is out of valid range" );
    }

    Map<String, HibTokenSetProperty> map = props[memberIndex];
    HibTokenSetProperty property = map.get( name );

    if ( property == null )
    {
      property = new HibTokenSetProperty( tokenSet, memberIndex, name, value );
      map.put( name, property );
      tokenSet.getProperties().add( property );
    }
    else
    {
      tokenSet.getProperties().remove( property );
    }
  }

  public void setStringAttribute (final String name,
                                  final List<String> values)
  {
    int idx = 0;
    Iterator<String> iter = values.iterator();
    while ( iter.hasNext() && idx < props.length )
    {
      setStringAttribute( idx, name, iter.next() );
      idx++;
    }
  }

  public void removeAttribute (final int memberIndex,
                               final String name)
  {
    if ( memberIndex < 0 || memberIndex >= props.length )
    {
      throw new IllegalArgumentException( "Given memberIndex of " + memberIndex + " is out of valid range" );
    }

    Map<String, HibTokenSetProperty> map = props[memberIndex];
    HibTokenSetProperty property = map.get( name );

    if ( property != null )
    {
      map.remove( name );
      tokenSet.getProperties().remove( property );
    }
  }

  public void removeAttribute (final String name)
  {
    for ( int index = 0; index < props.length; index++ )
    {
      removeAttribute( index, name );
    }
  }
}