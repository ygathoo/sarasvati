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

package org.codemonk.wf.mem;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class MemWfGraphCache
{
  private static Map<String,MemWfGraph> cache = new HashMap<String, MemWfGraph>();

  public static void addToCache (String name, MemWfGraph graph)
  {
    cache.put( name, graph );
  }

  public static MemWfGraph get (String name)
  {
    return cache.get( name );
  }

  public static List<MemWfGraph> getGraphs ()
  {
    return new ArrayList<MemWfGraph>( cache.values() );
  }
}
