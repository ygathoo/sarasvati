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
package com.googlecode.sarasvati.load;

import java.util.HashMap;
import java.util.Map;

import org.w3c.dom.Element;

import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.load.properties.DOMToObjectLoadHelper;

public class DefaultNodeFactory implements NodeFactory
{
  protected Map<String, Class<? extends Node>> typeMap = new HashMap<String, Class<? extends Node>>();
  protected Class<? extends Node> defaultClass;

  public DefaultNodeFactory (Class<? extends Node> defaultClass)
  {
    this.defaultClass = defaultClass;
  }

  public void addType (String type, Class<? extends Node> clazz)
  {
    typeMap.put( type, clazz );
  }

  @Override
  public void loadCustom (Node node, Object custom) throws LoadException
  {
    if ( custom == null )
    {
      return;
    }

    if ( custom instanceof Element )
    {
      DOMToObjectLoadHelper.setBeanValues( node, (Element)custom );
    }
    else
    {
      throw new LoadException( "Expected DOM Element, but got instance of " + custom.getClass().getName() +
                               " while loading node of type " + node.getType() );
    }
  }

  @Override
  public Node newNode (String type) throws LoadException
  {
    Class<? extends Node> clazz = typeMap.get( type );
    clazz = clazz == null ? defaultClass : clazz;

    try
    {
      return clazz.newInstance();
    }
    catch ( InstantiationException e )
    {
      throw new LoadException( "Unable to create new instance for type '" + type + "'", e );
    }
    catch ( IllegalAccessException e )
    {
      throw new LoadException( "Unable to create new instance for type '" + type + "'", e );
    }
  }
}