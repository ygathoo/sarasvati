package com.googlecode.sarasvati.jdbc.stmt;

import java.sql.ResultSet;
import java.sql.SQLException;

import com.googlecode.sarasvati.jdbc.JdbcGraph;

public abstract class AbstractGraphSelectStatementExecutor extends AbstractSelectStatementExecutor<JdbcGraph>
{
  public AbstractGraphSelectStatementExecutor (String sql)
  {
    super( sql );
  }

  @Override
  protected JdbcGraph loadObject (ResultSet row) throws SQLException
  {
    long graphId = row.getLong( 1 );
    String graphName = row.getString( 2 );
    int version = row.getInt( 3 );

    return new JdbcGraph( graphId, graphName, version );
  }
}
