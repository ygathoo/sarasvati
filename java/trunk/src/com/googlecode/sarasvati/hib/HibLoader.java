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

package com.googlecode.sarasvati.hib;

import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.BaseLoader;
import com.googlecode.sarasvati.ImportException;

public class HibLoader extends BaseLoader<HibEngine,HibGraph>
{
  public static interface NodeFactory
  {
    HibNode createNode( HibEngine engine, HibNode node, Object custom)
      throws ImportException;
  }

  protected Map<String,NodeFactory> customTypeFactories = new HashMap<String, NodeFactory>();

  public HibLoader (HibEngine engine)
  {
    super( engine );
  }

  public void addCustomType (String type, NodeFactory factory)
  {
    customTypeFactories.put( type, factory );
  }

  @Override
  protected HibNodeRef createNode (String name,
                                   String type,
                                   boolean isJoin,
                                   boolean isStart,
                                   String guard,
                                   Object custom)
    throws ImportException
  {
    HibNode node = new HibNode(getGraph(), name, type, isJoin, isStart, guard);

    NodeFactory factory = customTypeFactories.get( type );

    if ( factory == null )
    {
      engine.getSession().save( node );
    }
    else
    {
      node = factory.createNode( engine, node, custom );
    }

    HibNodeRef nodeRef = new HibNodeRef( getGraph(), node, "" );
    engine.getSession().save( nodeRef  );
    getGraph().getNodes().add( nodeRef );

    return nodeRef;
  }
}