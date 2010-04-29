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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import com.googlecode.sarasvati.load.GraphRepository;

public class MemGraphRepository implements GraphRepository<MemGraph>
{
  public static final MemGraphRepository INSTANCE = new MemGraphRepository();

  private static Map<String,MemGraph> cache = new ConcurrentHashMap<String, MemGraph>();

  @Override
  public void addGraph (final MemGraph graph)
  {
    cache.put( graph.getName(), graph );
  }

  @Override
  public List<MemGraph> getGraphs(final String name)
  {
    MemGraph graph = cache.get( name );
    if (  graph == null )
    {
      return Collections.emptyList();
    }
    return Collections.singletonList( graph );
  }

  @Override
  public List<MemGraph> getGraphs()
  {
    ArrayList<MemGraph> graphs = new ArrayList<MemGraph>( cache.size() );
    graphs.addAll( cache.values() );
    return graphs;
  }

  @Override
  public MemGraph getLatestGraph(final String name)
  {
    return cache.get( name );
  }
}
