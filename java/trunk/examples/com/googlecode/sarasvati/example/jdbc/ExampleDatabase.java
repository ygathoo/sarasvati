/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.example.jdbc;

import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;
import com.googlecode.sarasvati.jdbc.stmt.AbstractSelectStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractStatement;

public interface ExampleDatabase
{
  AbstractStatement newInsertTaskNodeStatement (JdbcExampleTaskNode taskNode);

  AbstractStatement newLoadTaskNodeStatement (JdbcExampleTaskNode taskNode);

  AbstractStatement newInsertTaskStatement (JdbcExampleTask task);

  JdbcExampleTask getTaskForToken (JdbcEngine engine, JdbcNodeToken token);

  AbstractSelectStatement<JdbcExampleTask> newTaskByTokenSelectStatement (JdbcNodeToken token);

  AbstractStatement newUpdateTaskStatement (JdbcExampleTask task);
}
