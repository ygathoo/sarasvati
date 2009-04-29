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
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.List;

import com.googlecode.sarasvati.jdbc.stmt.AbstractGraphSelectStatementExecutor;
import com.googlecode.sarasvati.jdbc.stmt.ArcSelectStatementExecutor;
import com.googlecode.sarasvati.jdbc.stmt.NodeSelectStatementExecutor;
import com.googlecode.sarasvati.load.GraphRepository;

public class JdbcGraphRepostitory implements GraphRepository<JdbcGraph>
{
  private static final String SELECT_LATEST_GRAPH_SQL =
    "select id, name, version from wf_graph " +
    " where name = ? and version in (select max(version) from wf_graph where name = ?)";

  private static final String SELECT_ALL_GRAPHS_SQL =
    "select id, name, version from wf_graph";

  private static final String SELECT_GRAPHS_BY_NAME_SQL =
    "select id, name, version from wf_graph where name = ?";

  private static final String SELECT_NODES_SQL =
    "select ref.id as ref_id, ref.instance, node.id, node.name, node.type, node.is_join, node.is_start, node.guard" +
    "  from wf_node_ref ref join wf_node node on ref.node_id = node.node_id " +
    " where ref.graph_id = ?";

  private static final String SELECT_ARCS_SQL =
    "select id, a_node_ref_id, z_node_ref_id from wf_arc where graph_id = ?";

  protected Connection connection;
  protected JdbcGraphFactory factory;

  public JdbcGraphRepostitory (Connection connection, JdbcGraphFactory factory)
  {
    this.connection = connection;
    this.factory = factory;
  }

  @Override
  public void addGraph (JdbcGraph graph)
  {
    // Does nothing, since this is handled by the DB
  }

  @Override
  public List<JdbcGraph> getGraphs (final String name)
  {
    AbstractGraphSelectStatementExecutor ex = new AbstractGraphSelectStatementExecutor( SELECT_GRAPHS_BY_NAME_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setString( 1, name );
      }
    };

    ex.execute( connection );

    return ex.getResult();
  }

  @Override
  public List<JdbcGraph> getGraphs ()
  {
    AbstractGraphSelectStatementExecutor ex = new AbstractGraphSelectStatementExecutor( SELECT_ALL_GRAPHS_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        // no parameters to set
      }
    };

    ex.execute( connection );

    return ex.getResult();
  }

  @Override
  public JdbcGraph getLatestGraph (final String name)
  {
    AbstractGraphSelectStatementExecutor ex = new AbstractGraphSelectStatementExecutor( SELECT_LATEST_GRAPH_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setString( 1, name );
        stmt.setString( 2, name );
      }
    };

    ex.execute( connection );

    return ex.getResult().isEmpty() ? null : ex.getResult().get( 0 );
  }

  protected void loadNodes (JdbcGraph graph)
  {
    new NodeSelectStatementExecutor( SELECT_NODES_SQL, graph, factory ).execute( connection );
  }

  public void loadArcs (JdbcGraph graph)
  {
    new ArcSelectStatementExecutor( SELECT_ARCS_SQL, graph ).execute( connection );
  }
}