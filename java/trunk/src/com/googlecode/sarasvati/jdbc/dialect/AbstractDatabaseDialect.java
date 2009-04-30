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
package com.googlecode.sarasvati.jdbc.dialect;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.jdbc.JdbcArc;
import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcGraphFactory;
import com.googlecode.sarasvati.jdbc.JdbcNode;
import com.googlecode.sarasvati.jdbc.JdbcNodeRef;
import com.googlecode.sarasvati.jdbc.stmt.AbstractExecuteUpdateStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractGraphSelectStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractSelectStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractStatement;
import com.googlecode.sarasvati.jdbc.stmt.ArcSelectStatement;
import com.googlecode.sarasvati.jdbc.stmt.NodeSelectStatement;


public abstract class AbstractDatabaseDialect implements DatabaseDialect
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
    "  from wf_node_ref ref join wf_node node on ref.node_id = node.id " +
    " where ref.graph_id = ?";

  private static final String SELECT_ARCS_SQL =
    "select id, a_node_ref_id, z_node_ref_id, name from wf_arc where graph_id = ?";

  private static final String INSERT_NODE_PROPERTY_SQL =
    "insert into wf_node_attr (node_id, name, value) values (?, ?, ?)";

  protected Map<Class<?>,Object> userData = new HashMap<Class<?>,Object> ();

  @Override
  public AbstractSelectStatement<JdbcArc> newArcSelectStatement (final JdbcGraph graph)
  {
    return new ArcSelectStatement( SELECT_ARCS_SQL, graph );
  }

  @Override
  public AbstractSelectStatement<JdbcGraph> newGraphByNameSelectStatement (final String name)
  {
    return new AbstractGraphSelectStatement( SELECT_GRAPHS_BY_NAME_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setString( 1, name );
      }
    };
  }

  @Override
  public AbstractSelectStatement<JdbcGraph> newGraphSelectStatement ()
  {
    return new AbstractGraphSelectStatement( SELECT_ALL_GRAPHS_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        // no parameters to set
      }
    };
  }

  @Override
  public AbstractSelectStatement<JdbcGraph> newLatestGraphByNameSelectStatement (final String name)
  {
    return new AbstractGraphSelectStatement( SELECT_LATEST_GRAPH_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setString( 1, name );
        stmt.setString( 2, name );
      }
    };
  }

  @Override
  public AbstractSelectStatement<JdbcNodeRef> newNodeSelectStatement (final JdbcGraph graph, final JdbcGraphFactory factory)
  {
    return new NodeSelectStatement( SELECT_NODES_SQL, graph, factory );
  }

  @Override
  public AbstractStatement newNodePropertyInsertStatement (final JdbcNode node, final String key, final String value)
  {
    return new AbstractExecuteUpdateStatement( INSERT_NODE_PROPERTY_SQL )
    {
      @Override
      protected void setParameters (PreparedStatement stmt) throws SQLException
      {
        stmt.setLong( 1, node.getId() );
        stmt.setString( 2, key );
        stmt.setString( 3, value );
      }
    };
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T getUserData (Class<T> key)
  {
    return (T)userData.get( key );
  }

  @Override
  public <T> void setUserData (Class<T> key, T value)
  {
    userData.put( key, value );
  }
}