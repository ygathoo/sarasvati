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

import java.sql.Connection;
import java.util.List;

import com.googlecode.sarasvati.jdbc.dialect.DatabaseDialect;
import com.googlecode.sarasvati.jdbc.stmt.AbstractSelectStatement;
import com.googlecode.sarasvati.load.GraphRepository;

public class JdbcGraphRepository implements GraphRepository<JdbcGraph>
{
  protected DatabaseDialect dialect;
  protected JdbcGraphFactory factory;
  protected Connection connection;

  public JdbcGraphRepository (final DatabaseDialect dialect, final JdbcGraphFactory factory, final Connection connection)
  {
    this.dialect = dialect;
    this.factory = factory;
    this.connection = connection;
  }

  @Override
  public void addGraph (JdbcGraph graph)
  {
    // Does nothing, since this is handled by the DB
  }

  @Override
  public List<JdbcGraph> getGraphs (final String name)
  {
    AbstractSelectStatement<JdbcGraph> stmt = dialect.newGraphByNameSelectStatement( name );
    stmt.execute( connection );

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
    AbstractSelectStatement<JdbcGraph> stmt = dialect.newGraphSelectStatement();
    stmt.execute( connection );

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
    AbstractSelectStatement<JdbcGraph> stmt = dialect.newLatestGraphByNameSelectStatement( name );
    stmt.execute( connection );

    for ( JdbcGraph graph :  stmt.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return stmt.getResult().isEmpty() ? null : stmt.getResult().get( 0 );
  }

  protected void loadNodes (final JdbcGraph graph)
  {
    AbstractSelectStatement<JdbcNodeRef> stmt = dialect.newNodeSelectStatement( graph, factory );
    stmt.execute( connection );
  }

  public void loadArcs (final JdbcGraph graph)
  {
    AbstractSelectStatement<JdbcArc> stmt = dialect.newArcSelectStatement( graph );
    stmt.execute( connection );
  }
}