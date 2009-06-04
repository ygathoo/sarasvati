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
package com.googlecode.sarasvati.jdbc.action;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.jdbc.JdbcArc;
import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcNodeRef;
import com.googlecode.sarasvati.load.LoadException;

public class ArcLoadAction extends AbstractLoadAction<JdbcArc>
{
  protected JdbcGraph graph;

  protected Map<Long, JdbcNodeRef> nodeRefMap = new HashMap<Long,JdbcNodeRef>();

  public ArcLoadAction (String sql, JdbcGraph graph)
  {
    super( sql, false );
    this.graph = graph;

    for ( JdbcNodeRef ref : graph.getNodes() )
    {
      nodeRefMap.put( ref.getId(), ref );
    }
  }

  @Override
  protected JdbcArc loadObject (ResultSet row) throws SQLException, LoadException
  {
    long arcId       = row.getLong( 1 );
    long startRefId  = row.getLong( 2 );
    long endRefId    = row.getLong( 3 );
    String name      = row.getString( 4 );

    JdbcNodeRef startRef = nodeRefMap.get( startRefId );
    JdbcNodeRef endRef   = nodeRefMap.get( endRefId );

    JdbcArc arc = new JdbcArc( arcId, graph, startRef, endRef, name );
    graph.getArcs().add( arc );
    return arc;
  }

  @Override
  protected void setParameters (PreparedStatement stmt) throws SQLException
  {
    stmt.setLong( 1, graph.getId() );
  }
}
