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

import java.util.Date;

import com.googlecode.sarasvati.jdbc.JdbcArc;
import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcGraphFactory;
import com.googlecode.sarasvati.jdbc.JdbcNode;
import com.googlecode.sarasvati.jdbc.JdbcNodeRef;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;
import com.googlecode.sarasvati.jdbc.stmt.AbstractInsertStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractSelectStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractStatement;

/**
 * Interface to perform database-specific logic, such as
 * inserts, updates, selects and deletes.
 *
 * @author Paul Lorenz
 */
public interface DatabaseDialect
{
  /**
   * Return an {@link AbstractInsertStatement} to
   * insert a new graph into the database.
   *
   * @param name The graph name
   * @param version The graph version
   *
   * @return The insert statement.
   */
  AbstractInsertStatement newGraphInsertStatement (String name, int version);

  /**
   * Return an {@link AbstractInsertStatement} to
   * insert a new process into the database.
   *
   * @param graph The graph being executed
   * @param parentToken If this is a nested process, this should be the token
   *                    which is causing the process to be started. Otherwise
   *                    it should be null.
   *
   * @return The insert statement.
   */
  AbstractInsertStatement newProcessInsertStatement (JdbcGraph graph, JdbcNodeToken parentToken, Date createDate);

  /**
   * Return an {@link AbstractInsertStatement} to
   * insert a new node into the database.
   *
   * @param graph The graph the node is being create for
   * @param name The node name
   * @param type The node type
   * @param guard The node guard
   * @param isStart Whether the node is a start node
   * @param isJoin Whether the node is a join node
   *
   * @return The insert statement
   */
  AbstractInsertStatement newNodeInsertStatement (JdbcGraph graph,
                                                  String name,
                                                  String type,
                                                  String guard,
                                                  boolean isStart,
                                                  boolean isJoin);

  /**
   * Returns an {@link AbstractInsertStatement} to
   * insert a new node ref into the database.
   *
   * @param graph The graph the node ref belongs to
   * @param node The node the node ref points to
   * @param instance The instance name associated with the node ref
   *
   * @return The insert statement
   */
  AbstractInsertStatement newNodeRefInsertStatement (JdbcGraph graph, JdbcNode node, String instance);

  /**
   * Returns an {@link AbstractInsertStatement} to
   * insert a new arc into the database.
   *
   * @param graph The graph the arc belongs to
   * @param startNode The arc's starting point
   * @param endNode The arc's ending point
   * @param name The arc name
   *
   * @return The insert statement
   */
  AbstractInsertStatement newArcInsertStatement (JdbcGraph graph,
                                                 JdbcNodeRef startNode,
                                                 JdbcNodeRef endNode,
                                                 String name );

  /**
   * Returns an {@link AbstractStatement} to
   * insert the node property into the database.
   *
   * @param node The node that the property is tied to
   * @param key The property key
   * @param value The property value
   *
   * @return The insert statement
   */
  AbstractStatement newNodePropertyInsertStatement (JdbcNode node, String key, String value);

  /**
   * Returns an {@link AbstractSelectStatement} which will load all graphs from the database.
   *
   * @return The select statement
   */
  AbstractSelectStatement<JdbcGraph> newGraphSelectStatement ();

  /**
   * Returns an {@link AbstractSelectStatement} which will load all graphs with the given name.
   *
   * @param name The name of the graphs to load
   *
   * @return The select statement
   */
  AbstractSelectStatement<JdbcGraph> newGraphByNameSelectStatement (String name);

  /**
   * Returns an {@link AbstractSelectStatement} which will load the latest graph with the given name.
   *
   * @param name The name of the graph to load
   *
   * @return The select statement
   */
  AbstractSelectStatement<JdbcGraph> newLatestGraphByNameSelectStatement (String name);

  /**
   * Returns an {@link AbstractSelectStatement} which will load nodes into the given graph.
   *
   * @param graph The graph whose nodes are to be loaded.
   *
   * @return The select statement
   */
  AbstractSelectStatement<JdbcNodeRef> newNodeSelectStatement (JdbcGraph graph, JdbcGraphFactory factory);

  /**
   * Returns an {@link AbstractSelectStatement} which will load arcs into the given graph.
   *
   * @param graph The graph whose arcs are to be loaded.
   *
   * @return The select statement
   */
  AbstractSelectStatement<JdbcArc> newArcSelectStatement (JdbcGraph graph);

  <T> void setUserData (Class<T> key, T value);

  <T> T getUserData (Class<T> key);
}