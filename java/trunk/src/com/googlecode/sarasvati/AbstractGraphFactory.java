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

public abstract class AbstractGraphFactory<G extends Graph> implements GraphFactory<G>
{
  protected Map<String, Class<? extends Node>> typeMap = new HashMap<String, Class<? extends Node>>();
  protected Class<? extends Node> defaultClass;

  public AbstractGraphFactory (Class<? extends Node> defaultClass)
  {
    this.defaultClass = defaultClass;
  }

  public void addType (String type, Class<? extends Node> clazz)
  {
    typeMap.put( type, clazz );
  }

  public Node newNode (String type) throws ImportException
  {
    Class<? extends Node> clazz = typeMap.get( type );
    clazz = clazz == null ? defaultClass : clazz;

    try
    {
      return clazz.newInstance();
    }
    catch ( InstantiationException e )
    {
      throw new ImportException( "Unable to create new instance for type '" + type + "'", e );
    }
    catch ( IllegalAccessException e )
    {
      throw new ImportException( "Unable to create new instance for type '" + type + "'", e );
    }
  }
}
