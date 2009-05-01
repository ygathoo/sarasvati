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
package com.googlecode.sarasvati.example.jdbc;

import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.example.TaskState;
import com.googlecode.sarasvati.jdbc.HasGeneratedId;
import com.googlecode.sarasvati.jdbc.JdbcNodeToken;

public class JdbcExampleTask implements HasGeneratedId
{
  protected Long id;
  protected JdbcNodeToken nodeToken;

  protected String name;
  protected String description;

  protected TaskState state;

  public JdbcExampleTask (JdbcNodeToken nodeToken, String name, String description, TaskState state)
  {
    this ( null, nodeToken, name, description, state );
  }

  public JdbcExampleTask (Long id, JdbcNodeToken nodeToken, String name, String description, TaskState state)
  {
    this.id = id;
    this.nodeToken = nodeToken;
    this.name = name;
    this.description = description;
    this.state = state;
  }

  public Long getId ()
  {
    return id;
  }

  @Override
  public void setId (Long id)
  {
    this.id = id;
  }

  public JdbcNodeToken getNodeToken ()
  {
    return nodeToken;
  }

  public void setNodeToken (JdbcNodeToken nodeToken)
  {
    this.nodeToken = nodeToken;
  }

  public String getName ()
  {
    return name;
  }

  public String getDescription ()
  {
    return description;
  }

  public TaskState getState ()
  {
    return state;
  }

  public void setState (TaskState state )
  {
    this.state = state;
  }

  public boolean isRejectable ()
  {
    Node node = getNodeToken().getNode();
    return !node.getGraph().getOutputArcs( node, "reject" ).isEmpty();
  }
}