/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.example.jdbc;

import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;
import com.googlecode.sarasvati.jdbc.action.DatabaseAction;
import com.googlecode.sarasvati.jdbc.action.DatabaseLoadAction;

public interface ExampleActionFactory
{
  DatabaseAction newInsertTaskNodeAction (JdbcExampleTaskNode taskNode);

  DatabaseAction newLoadTaskNodeAction (JdbcExampleTaskNode taskNode);

  DatabaseAction newInsertTaskAction (JdbcExampleTask task);

  JdbcExampleTask getTaskForToken (JdbcEngine engine, JdbcNodeToken token);

  DatabaseLoadAction<JdbcExampleTask> newTaskByTokenSelectAction (JdbcNodeToken token);

  DatabaseAction newUpdateTaskAction (JdbcExampleTask task);
}
