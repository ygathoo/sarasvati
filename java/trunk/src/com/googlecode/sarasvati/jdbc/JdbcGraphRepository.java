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

import com.googlecode.sarasvati.jdbc.action.DatabaseLoadAction;
import com.googlecode.sarasvati.jdbc.dialect.DatabaseDialect;
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
    DatabaseLoadAction<JdbcGraph> action = getDialect().newGraphByNameLoadAction( name );
    action.execute( engine );

    for ( JdbcGraph graph :  action.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return action.getResult();
  }

  @Override
  public List<JdbcGraph> getGraphs ()
  {
    DatabaseLoadAction<JdbcGraph> action = getDialect().newGraphLoadAction();
    action.execute( engine );

    for ( JdbcGraph graph :  action.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return action.getResult();
  }

  @Override
  public JdbcGraph getLatestGraph (final String name)
  {
    DatabaseLoadAction<JdbcGraph> action = getDialect().newLatestGraphByNameLoadAction( name );
    action.execute( engine );

    for ( JdbcGraph graph :  action.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return action.getFirstResult();
  }

  public JdbcGraph getGraph (final long graphId)
  {
    DatabaseLoadAction<JdbcGraph> action = getDialect().newGraphByIdLoadAction( graphId );
    action.execute( engine );

    for ( JdbcGraph graph :  action.getResult() )
    {
      loadNodes( graph );
      loadArcs( graph );
    }

    return action.getFirstResult();
  }

  public JdbcGraphProcess loadProcess (final long processId)
  {
    DatabaseLoadAction<JdbcGraphProcess> action = getDialect().newProcessLoadAction( processId, engine );
    action.execute( engine );
    return action.getFirstResult();
  }

  protected void loadNodes (final JdbcGraph graph)
  {
    DatabaseLoadAction<JdbcNodeRef> stmt = getDialect().newNodeLoadAction( graph, engine );
    stmt.execute( engine );
  }

  public void loadArcs (final JdbcGraph graph)
  {
    DatabaseLoadAction<JdbcArc> action = getDialect().newArcLoadAction( graph );
    action.execute( engine );
  }
}