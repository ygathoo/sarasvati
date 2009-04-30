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

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import com.googlecode.sarasvati.jdbc.JdbcLoadException;

public abstract class AbstractInsertStatement extends AbstractStatement
{
  private Long generatedId;

  public AbstractInsertStatement (String sql)
  {
    super( sql );
  }

  @Override
  public void doWork () throws SQLException
  {
    setParameters( getStatement() );
    executeQuery();
    ResultSet rs = getResultSet();
    if ( !rs.next() )
    {
      throw new JdbcLoadException( "No id returned from insert!" );
    }
    generatedId = rs.getLong( 1 );
  }

  protected abstract void setParameters (PreparedStatement stmt) throws SQLException;

  public Long getGeneratedId ()
  {
    return generatedId;
  }
}
