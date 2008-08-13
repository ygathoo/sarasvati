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

package com.googlecode.sarasvati.mem;

import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.BaseLoader;
import com.googlecode.sarasvati.ImportException;

public class MemLoader extends BaseLoader<MemEngine,MemGraph>
{
  public static interface NodeFactory
  {
    MemNode createNode (MemNode node, Object custom)
      throws ImportException;
  }

  protected Map<String,NodeFactory> customTypeFactories = new HashMap<String, NodeFactory>();

  public MemLoader()
  {
    super( new MemEngine() );
  }

  public void addCustomType (String type, NodeFactory factory)
  {
    customTypeFactories.put( type, factory );
  }

  @Override
  protected MemNode createNode (String name,
                                String type,
                                boolean isJoin,
                                boolean isStart,
                                String guard,
                                Object custom)
    throws ImportException
  {
    MemNode node = new MemNode( getGraph(), name, type, isJoin, isStart, guard );

    NodeFactory factory = customTypeFactories.get( type );

    if ( factory != null )
    {
      node = factory.createNode( node, custom );
    }

    getGraph().getNodes().add( node );
    return node;
  }
}