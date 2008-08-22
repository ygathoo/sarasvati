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
package com.googlecode.sarasvati.load.properties;

import java.beans.PropertyDescriptor;
import java.util.HashMap;
import java.util.Map;

public class PropertyMutatorRegistry
{
  protected static Map<Class<?>, Class<?>> mutatorMap = new HashMap<Class<?>, Class<?>>();

  static
  {
    mutatorMap.put( Boolean.TYPE, BooleanPropertyMutator.class );
    mutatorMap.put( Boolean.class, BooleanPropertyMutator.class );

    mutatorMap.put( Integer.TYPE, IntegerPropertyMutator.class );
    mutatorMap.put( Integer.class, IntegerPropertyMutator.class );

    mutatorMap.put( Long.TYPE, LongPropertyMutator.class );
    mutatorMap.put( Long.class, LongPropertyMutator.class );

    mutatorMap.put( String.class, StringPropertyMutator.class );
  }

  public static void registerPropertyMutator (Class<?> targetClass, Class<?> mutatorClass)
  {
    mutatorMap.put( targetClass, mutatorClass );
  }

  public static PropertyMutator getMutator (PropertyDescriptor pd, Object obj, PropertyMutator defaultMutator)
  {
    Class<?> mutatorClass = mutatorMap.get( pd.getPropertyType() );

    PropertyMutator mutator = null;

    if ( mutatorClass != null )
    {
      try
      {
        mutator = (PropertyMutator)mutatorClass.newInstance();
      }
      catch (Throwable t)
      {
        mutator = null; // use the default property mutator
      }
    }

    mutator = mutator != null ? mutator : defaultMutator;
    mutator.setPropertyDescriptor( pd );
    mutator.setTarget( obj );

    return mutator;
  }
}
