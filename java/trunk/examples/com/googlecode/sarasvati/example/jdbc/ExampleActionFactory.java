/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.example.jdbc;

import java.util.List;

import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcGraphProcess;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;
import com.googlecode.sarasvati.jdbc.action.DatabaseAction;
import com.googlecode.sarasvati.jdbc.action.DatabaseLoadAction;

public interface ExampleActionFactory
{
  DatabaseAction newInsertTaskNodeAction (JdbcExampleTaskNode taskNode);

  DatabaseAction newLoadTaskNodeAction (JdbcExampleTaskNode taskNode);

  DatabaseAction newInsertTaskAction (JdbcExampleTask task);

  JdbcExampleTask getTaskForToken (JdbcEngine engine, JdbcNodeToken token);

  DatabaseLoadAction<JdbcExampleTask> newLoadTaskByTokenAction (JdbcNodeToken token);

  DatabaseAction newUpdateTaskAction (JdbcExampleTask task);

  List<JdbcExampleTask> loadTasksForProcess (JdbcEngine engine, JdbcGraphProcess process);

  DatabaseLoadAction<JdbcExampleTask> newLoadTasksByProcessAction (JdbcGraphProcess process);
}
