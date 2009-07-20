/**
 * Created on Apr 30, 2009
 */
package com.googlecode.sarasvati.example.jdbc;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import com.googlecode.sarasvati.example.TaskState;
import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcGraphProcess;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;
import com.googlecode.sarasvati.jdbc.action.AbstractDatabaseAction;
import com.googlecode.sarasvati.jdbc.action.AbstractExecuteUpdateAction;
import com.googlecode.sarasvati.jdbc.action.AbstractInsertAction;
import com.googlecode.sarasvati.jdbc.action.AbstractLoadAction;
import com.googlecode.sarasvati.jdbc.action.DatabaseAction;
import com.googlecode.sarasvati.jdbc.action.DatabaseLoadAction;

public class BaseExampleActionFactory implements ExampleActionFactory
{
  private static final String INSERT_TASK_NODE_SQL =
    "insert into wf_node_task ( id, name, description ) values ( ?, ?, ? )";

  private static final String SELECT_TASK_NODE_SQL =
    "select name, description from wf_node_task where id = ?";

  private static final String INSERT_TASK_SQL =
    "insert into wf_task (node_token_id, name, description, state) values (?, ?, ?, 0) returning id";

  private static final String SELECT_TASK_BY_TOKEN_SQL =
    "select id, name, description, state from wf_task where node_token_id = ?";

  private static final String UPDATE_TASK_SQL =
    "update wf_task set name = ?, description = ?, state = ? where id = ?";

  private static final String SELECT_TASKS_BY_PROCESS_SQL =
    "select t.id, t.node_token_id, t.name, t.description, t.state " +
    "  from wf_task t " +
    "  join wf_node_token nt on t.node_token_id = nt.id " +
    "  join wf_process p on nt.process_id = p.id " +
    " where p.id = ? or " +
    "       p.parent_token_id in (select nt2.id from wf_node_token nt2 where nt2.process_id = ?)";

  @Override
  public DatabaseAction newInsertTaskNodeAction (final JdbcExampleTaskNode taskNode)
  {
    return new AbstractExecuteUpdateAction( INSERT_TASK_NODE_SQL )
    {
      @Override
      protected void setParameters (final PreparedStatement stmt) throws SQLException
      {
        stmt.setLong( 1, taskNode.getId() );
        stmt.setString( 2, taskNode.getTaskName() );
        stmt.setString( 3, taskNode.getTaskDesc() );
      }
    };
  }

  @Override
  public DatabaseAction newLoadTaskNodeAction (final JdbcExampleTaskNode taskNode)
  {
    return new AbstractLoadAction<Object>( SELECT_TASK_NODE_SQL, false )
    {
      @Override
      protected Object loadObject (ResultSet row) throws SQLException
      {
        taskNode.setTaskName( row.getString( 1 ) );
        taskNode.setTaskDesc( row.getString( 2 ) );
        return null;
      }

      @Override
      protected void setParameters (final PreparedStatement stmt) throws SQLException
      {
        stmt.setLong( 1, taskNode.getId() );
      }
    };
  }

  @Override
  public DatabaseAction newInsertTaskAction (final JdbcExampleTask task)
  {
    return new AbstractInsertAction<JdbcExampleTask>( INSERT_TASK_SQL, task )
    {
      @Override
      protected void setParameters (final PreparedStatement stmt) throws SQLException
      {
        stmt.setLong( 1, task.getNodeToken().getId() );
        stmt.setString( 2, task.getName() );
        stmt.setString( 3, task.getDescription() );
      }
    };
  }

  @Override
  public JdbcExampleTask getTaskForToken (final JdbcEngine engine, final JdbcNodeToken token)
  {
    AbstractLoadAction<JdbcExampleTask> stmt = newLoadTaskByTokenAction( token );
    stmt.execute( engine );
    return stmt.getFirstResult();
  }

  @Override
  public AbstractLoadAction<JdbcExampleTask> newLoadTaskByTokenAction (final JdbcNodeToken token)
  {
    return new AbstractLoadAction<JdbcExampleTask>( SELECT_TASK_BY_TOKEN_SQL, true )
    {
      @Override
      protected JdbcExampleTask loadObject (ResultSet row) throws SQLException
      {
        long id = row.getLong( 1 );
        String name = row.getString( 2 );
        String description = row.getString( 3 );
        TaskState state = TaskState.getById( row.getInt( 4 ) );
        return new JdbcExampleTask( id, token, name, description, state );
      }

      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setLong( 1, token.getId() );
      }
    };
  }

  @Override
  public List<JdbcExampleTask> loadTasksForProcess (JdbcEngine engine, JdbcGraphProcess process)
  {
    DatabaseLoadAction<JdbcExampleTask> action = newLoadTasksByProcessAction( process );
    action.execute( engine );
    return action.getResult();
  }

  @Override
  public AbstractLoadAction<JdbcExampleTask> newLoadTasksByProcessAction (final JdbcGraphProcess process)
  {
    return new AbstractLoadAction<JdbcExampleTask>( SELECT_TASKS_BY_PROCESS_SQL, true )
    {
      @Override
      protected JdbcExampleTask loadObject (ResultSet row) throws SQLException
      {
        long id = row.getLong( 1 );
        String name = row.getString( 2 );
        String description = row.getString( 3 );
        TaskState state = TaskState.getById( row.getInt( 4 ) );
        return new JdbcExampleTask( id, null, name, description, state );
      }

      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setLong( 1, process.getId() );
        stmt.setLong( 2, process.getId() );
      }
    };
  }

  @Override
  public AbstractDatabaseAction newUpdateTaskAction (final JdbcExampleTask task)
  {
    return new AbstractExecuteUpdateAction( UPDATE_TASK_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setString( 1, task.getName() );
        stmt.setString( 2, task.getDescription() );
        stmt.setInt( 3, task.getState().getId() );
      }
    };
  }
}