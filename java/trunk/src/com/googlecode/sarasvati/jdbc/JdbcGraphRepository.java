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

import com.googlecode.sarasvati.jdbc.dialect.DatabaseDialect;
import com.googlecode.sarasvati.jdbc.stmt.AbstractSelectStatement;
import com.googlecode.sarasvati.load.GraphRepository;

public class JdbcGraphRepository implements GraphRepository<JdbcGraph>
{
  private final JdbcEngine engine;

  public JdbcGraphRepository (final JdbcEngine engine)
  {
    this.engine = engine;
  }

  private DatabaseDialect getDialect ()
  {
    return engine.getDatabaseDialect();
  }

  @Override
  public void addGraph (JdbcGraph graph)
  {
    // Does nothing, since this is handled by the DB
  }

  @Override
  public List<JdbcGraph> getGraphs (final String name)
  {
    AbstractSelectStatement<JdbcGraph> stmt = getDialect().newGraphByNameSelectStatement( name );
    stmt.execute( engine );

    for ( JdbcGraph graph :  stmt.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return stmt.getResult();
  }

  @Override
  public List<JdbcGraph> getGraphs ()
  {
    AbstractSelectStatement<JdbcGraph> stmt = getDialect().newGraphSelectStatement();
    stmt.execute( engine );

    for ( JdbcGraph graph :  stmt.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return stmt.getResult();
  }

  @Override
  public JdbcGraph getLatestGraph (final String name)
  {
    AbstractSelectStatement<JdbcGraph> stmt = getDialect().newLatestGraphByNameSelectStatement( name );
    stmt.execute( engine );

    for ( JdbcGraph graph :  stmt.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return stmt.getFirstResult();
  }

  protected void loadNodes (final JdbcGraph graph)
  {
    AbstractSelectStatement<JdbcNodeRef> stmt = getDialect().newNodeSelectStatement( graph, engine );
    stmt.execute( engine );
  }

  public void loadArcs (final JdbcGraph graph)
  {
    AbstractSelectStatement<JdbcArc> stmt = getDialect().newArcSelectStatement( graph );
    stmt.execute( engine );
  }
}