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

import com.googlecode.sarasvati.BaseEngine;
import com.googlecode.sarasvati.jdbc.dialect.DatabaseDialect;
import com.googlecode.sarasvati.load.GraphLoader;

public class JdbcEngine extends BaseEngine
{
  protected JdbcGraphFactory factory;
  protected JdbcGraphRepository repository;
  protected Connection connection;
  protected DatabaseDialect dialect;

  public JdbcEngine ()
  {
    // default constructor
  }

  public JdbcEngine (final Connection connection, final DatabaseDialect dialect)
  {
    this.connection = connection;
    this.dialect = dialect;
    this.factory = new JdbcGraphFactory( this );
    this.repository = new JdbcGraphRepository( this );
  }

  @Override
  protected JdbcEngine newEngine ()
  {
    JdbcEngine engine =  new JdbcEngine();
    engine.connection = connection;
    engine.factory = factory;
    engine.repository = repository;
    return engine;
  }

  @Override
  public JdbcGraphFactory getFactory ()
  {
    return factory;
  }

  @Override
  public GraphLoader<JdbcGraph> getLoader ()
  {
    return new GraphLoader<JdbcGraph>( factory, repository );
  }

  @Override
  public JdbcGraphRepository getRepository ()
  {
    return repository;
  }

  public DatabaseDialect getDatabaseDialect ()
  {
    return dialect;
  }

  public Connection getConnection ()
  {
    return connection;
  }
}