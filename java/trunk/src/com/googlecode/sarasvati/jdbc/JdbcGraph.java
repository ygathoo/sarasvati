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
package com.googlecode.sarasvati.jdbc;

import java.util.LinkedList;
import java.util.List;

import com.googlecode.sarasvati.AbstractGraph;
import com.googlecode.sarasvati.Arc;

public class JdbcGraph extends AbstractGraph
{
  protected long   id;
  protected String name;
  protected int    version;

  protected List<JdbcNodeRef> nodes;
  protected List<Arc> arcs;

  public JdbcGraph (long id, String name, int version)
  {
    this.id      = id;
    this.name    = name;
    this.version = version;
    this.nodes   = new LinkedList<JdbcNodeRef>();
  }

  public long getId ()
  {
    return id;
  }

  @Override
  public List< ? extends Arc> getArcs ()
  {
    return arcs;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  @Override
  public List<JdbcNodeRef> getNodes ()
  {
    return nodes;
  }

  @Override
  public int getVersion ()
  {
    return version;
  }
}
