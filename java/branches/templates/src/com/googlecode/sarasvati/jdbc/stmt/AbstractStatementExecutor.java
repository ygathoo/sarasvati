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
package com.googlecode.sarasvati.jdbc.stmt;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.googlecode.sarasvati.jdbc.JdbcLoadException;

public abstract class AbstractStatementExecutor
{
  private final String sql;

  private PreparedStatement statement;
  private ResultSet resultSet;

  public AbstractStatementExecutor (String sql)
  {
    this.sql = sql;
  }

  public void execute (final Connection connection)
  {
    SQLException ex = null;
    try
    {
      statement = connection.prepareStatement( sql );
      doWork();
    }
    catch ( SQLException sqle )
    {
      ex = sqle;
    }

    if ( resultSet != null )
    {
      try
      {
        resultSet.close();
      }
      catch ( SQLException sqle )
      {
        if ( ex != null )
        {
          ex.setNextException( sqle );
        }
        else
        {
          ex = sqle;
        }
      }
    }

    if ( statement != null )
    {
      try
      {
        statement.close();
      }
      catch ( SQLException sqle )
      {
        if ( ex != null )
        {
          ex.setNextException( sqle );
        }
        else
        {
          ex = sqle;
        }
      }
    }

    if ( ex != null )
    {
      throw new JdbcLoadException( "Database exception", ex );
    }
  }

  protected void executeQuery () throws SQLException
  {
    resultSet = statement.executeQuery();
  }

  protected PreparedStatement getStatement ()
  {
    return statement;
  }

  protected ResultSet getResultSet ()
  {
    return resultSet;
  }

  public abstract void doWork () throws SQLException;
}