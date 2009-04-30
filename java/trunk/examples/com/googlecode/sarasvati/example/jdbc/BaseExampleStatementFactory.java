/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.example.jdbc;

import java.sql.PreparedStatement;
import java.sql.SQLException;

import com.googlecode.sarasvati.jdbc.stmt.AbstractExecuteUpdateStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractStatement;

public class BaseExampleStatementFactory implements ExampleStatementFactory
{
  private static final String INSERT_TASK_NODE_SQL =
    "insert into wf_node_task ( id, name, description ) values ( ?, ?, ? )";

  @Override
  public AbstractStatement newInsertTaskNodeStatement (final JdbcExampleTaskNode taskNode)
  {
    return new AbstractExecuteUpdateStatement( INSERT_TASK_NODE_SQL )
    {

      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setLong( 1, taskNode.getId() );
        stmt.setString( 2, taskNode.getTaskName() );
        stmt.setString( 3, taskNode.getTaskDesc() );
      }
    };
  }
}
