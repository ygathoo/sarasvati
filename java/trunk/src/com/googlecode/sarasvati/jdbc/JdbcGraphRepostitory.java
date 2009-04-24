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

import java.util.List;

import com.googlecode.sarasvati.load.GraphRepository;

public class JdbcGraphRepostitory implements GraphRepository<JdbcGraph>
{
  @Override
  public void addGraph (JdbcGraph graph)
  {
    // Does nothing, since this is handled by the DB
  }

  @Override
  public List<JdbcGraph> getGraphs (String name)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public List<JdbcGraph> getGraphs ()
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public JdbcGraph getLatestGraph (String name)
  {
    // TODO Auto-generated method stub
    return null;
  }
}
