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

public class JdbcGraph extends AbstractGraph implements JdbcObject
{
  protected Long   id;
  protected String name;
  protected int    version;

  protected List<JdbcNodeRef> nodes;
  protected List<JdbcArc> arcs;

  public JdbcGraph (String name, int version)
  {
    this( null, name, version );
  }

  public JdbcGraph (Long id, String name, int version)
  {
    this.id      = id;
    this.name    = name;
    this.version = version;
    this.nodes   = new LinkedList<JdbcNodeRef>();
    this.arcs    = new LinkedList<JdbcArc>();
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  @Override
  public List<JdbcArc> getArcs ()
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

  @Override
  public boolean isMutable ()
  {
    return false;
  }

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (!(obj instanceof JdbcGraph))
      return false;
    JdbcGraph other = (JdbcGraph) obj;
    if (id == null)
    {
      if (other.id != null)
        return false;
    } else if (!id.equals( other.id ))
      return false;
    return true;
  }
}
