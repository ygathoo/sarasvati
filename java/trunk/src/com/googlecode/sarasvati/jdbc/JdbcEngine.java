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
import com.googlecode.sarasvati.load.GraphFactory;
import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.load.GraphRepository;

public class JdbcEngine extends BaseEngine
{
  protected JdbcGraphFactory factory;
  protected JdbcGraphRepository repository;
  protected Connection connection;

  public JdbcEngine ()
  {
    // default constructor
  }

  public JdbcEngine (final Connection connection, final DatabaseDialect dialect)
  {
    this.connection = connection;
    this.factory = new JdbcGraphFactory( dialect, connection );
    this.repository = new JdbcGraphRepository( dialect, factory, connection );
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
  public GraphFactory<JdbcGraph> getFactory ()
  {
    return factory;
  }

  @Override
  public GraphLoader<JdbcGraph> getLoader ()
  {
    return new GraphLoader<JdbcGraph>( factory, repository );
  }

  @Override
  public GraphRepository<JdbcGraph> getRepository ()
  {
    return repository;
  }
}