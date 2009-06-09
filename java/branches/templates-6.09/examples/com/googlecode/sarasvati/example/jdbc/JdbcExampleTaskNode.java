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
package com.googlecode.sarasvati.example.jdbc;

import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.env.Env;
import com.googlecode.sarasvati.example.TaskState;
import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcNode;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;
import com.googlecode.sarasvati.jdbc.action.DatabaseAction;
import com.googlecode.sarasvati.jdbc.dialect.DatabaseDialect;

public class JdbcExampleTaskNode extends JdbcNode
{
  protected String taskName;

  protected String taskDesc;

  public JdbcExampleTaskNode() { /* Default constructor for Hibernate */ }

  public String getTaskName ()
  {
    return taskName;
  }

  public void setTaskName (String taskName)
  {
    this.taskName = taskName;
  }

  public String getTaskDesc ()
  {
    return taskDesc;
  }

  public void setTaskDesc (String taskDesc)
  {
    this.taskDesc = taskDesc;
  }

  @Override
  public void afterCreate (final JdbcEngine engine)
  {
    DatabaseDialect dialect = engine.getDatabaseDialect();
    ExampleActionFactory factory = dialect.getUserData( ExampleActionFactory.class );
    DatabaseAction stmt = factory.newInsertTaskNodeAction( this );
    stmt.execute( engine );
  }

  @Override
  public void afterLoad (final JdbcEngine engine)
  {
    DatabaseDialect dialect = engine.getDatabaseDialect();
    ExampleActionFactory factory = dialect.getUserData( ExampleActionFactory.class );
    DatabaseAction stmt = factory.newLoadTaskNodeAction( this );
    stmt.execute( engine );
  }

  @Override
  public void backtrack (final Engine engine, final NodeToken token)
  {
    JdbcEngine jdbcEngine = (JdbcEngine)engine;
    DatabaseDialect dialect = jdbcEngine.getDatabaseDialect();
    ExampleActionFactory db = dialect.getUserData( ExampleActionFactory.class );

    JdbcExampleTask task = db.getTaskForToken( jdbcEngine, (JdbcNodeToken)token );
    task.setState( TaskState.Cancelled );
    db.newUpdateTaskAction( task ).execute( jdbcEngine );
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T getAdaptor (final Class<T> clazz)
  {
    if ( String.class == clazz )
    {
      return (T)getTaskName();
    }
    return super.getAdaptor( clazz );
  }

  @Override
  public void execute (final Engine engine, final NodeToken token)
  {
    JdbcEngine jdbcEngine = (JdbcEngine)engine;

    JdbcExampleTask task = new JdbcExampleTask( (JdbcNodeToken)token, getTaskName(), getTaskDesc(), TaskState.Open );
    ExampleActionFactory factory = jdbcEngine.getDatabaseDialect().getUserData( ExampleActionFactory.class );
    factory.newInsertTaskAction( task ).execute( jdbcEngine );

    Env env = token.getEnv();
    env.setAttribute( task.getName(), env.getAttribute( task.getName(), Long.class ) + 1 );
    env = token.getProcess().getEnv();
    env.setAttribute( task.getName(), env.getAttribute( task.getName(), Long.class ) + 1 );
  }
}