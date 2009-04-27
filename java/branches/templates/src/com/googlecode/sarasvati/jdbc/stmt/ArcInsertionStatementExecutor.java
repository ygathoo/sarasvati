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
import com.googlecode.sarasvati.jdbc.JdbcNodeRef;


public class ArcInsertionStatementExecutor extends AbstractInsertionStatementExecutor
{
  protected JdbcGraph graph;
  protected JdbcNodeRef startNode;
  protected JdbcNodeRef endNode;
  protected String name;

  public ArcInsertionStatementExecutor (String sql, JdbcGraph graph, JdbcNodeRef startNode, JdbcNodeRef endNode, String name)
  {
    super( sql );
    this.graph = graph;
    this.startNode = startNode;
    this.endNode = endNode;
    this.name = name;
  }

  @Override
  protected void setParameters (PreparedStatement stmt) throws SQLException
  {
    stmt.setLong( 1, graph.getId() );
    stmt.setLong( 2, startNode.getId() );
    stmt.setLong( 3, endNode.getId() );
    stmt.setString( 4, name );
  }
}