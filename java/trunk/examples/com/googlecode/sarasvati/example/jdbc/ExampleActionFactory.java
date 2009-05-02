/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.example.jdbc;

import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;
import com.googlecode.sarasvati.jdbc.stmt.AbstractLoadAction;
import com.googlecode.sarasvati.jdbc.stmt.AbstractDatabaseAction;

public interface ExampleActionFactory
{
  AbstractDatabaseAction newInsertTaskNodeAction (JdbcExampleTaskNode taskNode);

  AbstractDatabaseAction newLoadTaskNodeAction (JdbcExampleTaskNode taskNode);

  AbstractDatabaseAction newInsertTaskAction (JdbcExampleTask task);

  JdbcExampleTask getTaskForToken (JdbcEngine engine, JdbcNodeToken token);

  AbstractLoadAction<JdbcExampleTask> newTaskByTokenSelectAction (JdbcNodeToken token);

  AbstractDatabaseAction newUpdateTaskAction (JdbcExampleTask task);
}
