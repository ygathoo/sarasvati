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
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.Date;

import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;

public class ProcessInsertStatement extends AbstractInsertStatement
{
  protected final JdbcGraph graph;
  protected final JdbcNodeToken parent;
  protected final Date createDate;

  public ProcessInsertStatement (final String sql,
                                 final JdbcGraph graph,
                                 final JdbcNodeToken parent,
                                 final Date createDate)
  {
    super( sql );
    this.graph = graph;
    this.parent = parent;
    this.createDate = createDate;
  }

  @Override
  protected void setParameters (final PreparedStatement stmt) throws SQLException
  {
    stmt.setLong( 1, graph.getId() );

    if ( parent == null )
    {
      stmt.setNull( 2, Types.BIGINT );
    }
    else
    {
      stmt.setLong( 2, parent.getId() );
    }

    stmt.setTimestamp( 3, new Timestamp( createDate.getTime() ) );
  }
}
