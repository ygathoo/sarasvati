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

import com.googlecode.sarasvati.jdbc.JdbcArc;
import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcGraphProcess;
import com.googlecode.sarasvati.jdbc.JdbcNode;
import com.googlecode.sarasvati.jdbc.JdbcNodeRef;
import com.googlecode.sarasvati.jdbc.JdbcPropertyNode;
import com.googlecode.sarasvati.jdbc.stmt.AbstractInsertStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractSelectStatement;
import com.googlecode.sarasvati.jdbc.stmt.AbstractStatement;
import com.googlecode.sarasvati.jdbc.stmt.NodePropertyLoadStatement;

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
   * @param graph The graph to insert
   *
   * @return The insert statement.
   */
  AbstractStatement newGraphInsertStatement (JdbcGraph graph);

  /**
   * Return an {@link AbstractInsertStatement} to
   * insert a new process into the database.
   *
   * @param process The graph process to insert
   *
   * @return The insert statement.
   */
  AbstractStatement newProcessInsertStatement (JdbcGraphProcess process);

  /**
   * Return an {@link AbstractInsertStatement} to
   * insert a new node into the database.
   *
   * @param node The node to insert
   *
   * @return The insert statement
   */
  AbstractStatement newNodeInsertStatement (JdbcNode node);

  /**
   * Returns an {@link AbstractInsertStatement} to
   * insert a new node ref into the database.
   *
   * @param nodeRef The node ref to insert
   *
   * @return The insert statement
   */
  AbstractStatement newNodeRefInsertStatement (JdbcNodeRef nodeRef);

  /**
   * Returns an {@link AbstractInsertStatement} to
   * insert a new arc into the database.
   *
   * @param arc The arc to insert
   *
   * @return The insert statement
   */
  AbstractStatement newArcInsertStatement (JdbcArc arc );

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
   * Returns a {@link NodePropertyLoadStatement} to load the node's associated
   * properties.
   *
   * @param node The {@link JdbcPropertyNode} whose properties need to be loaded.
   *
   * @return The load statement
   */
  AbstractStatement newNodePropertiesLoadStatement (JdbcPropertyNode node);

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
   * @param engine The engine to use to help instantiate nodes
   *
   * @return The select statement
   */
  AbstractSelectStatement<JdbcNodeRef> newNodeSelectStatement (JdbcGraph graph, JdbcEngine engine);

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