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
package com.googlecode.sarasvati.jdbc;

import java.sql.Connection;

public class PostgreSQLGraphFactory extends JdbcGraphFactory
{
  private static final String GRAPH_INSERT_SQL =
    "insert into wf_graph (name, version) values ( ?, ? ) returning id";

  private static final String NODE_INSERT_SQL =
    "insert into wf_node (graph_id, name, type, guard, is_start, is_join) values ( ?, ?, ?, ?, ?, ? ) returning id";

  private static final String NODE_REF_INSERT_SQL =
    "insert into wf_node_ref (graph_id, node_id, instance ) values ( ?, ?, ? ) returning id";

  private static final String ARC_INSERT_SQL =
    "insert into wf_arc (graph_id, a_node_ref_id, z_node_ref_id, name ) values ( ?, ?, ?, ? ) returning id";


  public PostgreSQLGraphFactory (Connection connection)
  {
    super( connection );
  }

  @Override
  protected String getGraphInsertionSql ()
  {
    return GRAPH_INSERT_SQL;
  }

  @Override
  protected String getNodeInsertionSql ()
  {
    return NODE_INSERT_SQL;
  }

  @Override
  protected String getArcInsertionSql ()
  {
    return ARC_INSERT_SQL;
  }

  @Override
  protected String getNodeRefInsertionSql ()
  {
    return NODE_REF_INSERT_SQL;
  }
}