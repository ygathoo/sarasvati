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

import com.googlecode.sarasvati.jdbc.JdbcGraph;


public class NodeInsertionStatementExecutor extends AbstractInsertionStatementExecutor
{
  protected JdbcGraph graph;
  protected String name;
  protected String type;
  protected String guard;
  protected boolean isStart;
  protected boolean isJoin;

  public NodeInsertionStatementExecutor (String sql,
                                         JdbcGraph graph,
                                         String name,
                                         String type,
                                         String guard,
                                         boolean isStart,
                                         boolean isJoin)
  {
    super( sql );
    this.graph = graph;
    this.name = name;
    this.type = type;
    this.guard = guard;
    this.isStart = isStart;
    this.isJoin = isJoin;
  }

  @Override
  protected void setParameters (PreparedStatement stmt) throws SQLException
  {
    stmt.setLong( 1, graph.getId() );
    stmt.setString( 2, name );
    stmt.setString( 3, type );
    stmt.setString( 4, guard );
    stmt.setBoolean( 5, isStart );
    stmt.setBoolean( 5, isJoin );
  }
}