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
package com.googlecode.sarasvati.hib;

import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;

import com.googlecode.sarasvati.load.GraphRepository;

public class HibGraphRepository implements GraphRepository<HibGraph>
{
  protected Session session;

  HibGraphRepository (Session session)
  {
    this.session = session;
  }

  @Override
  public void addGraph (HibGraph graph)
  {
    // does nothing, as graph is added to database when it is created
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<HibGraph> getGraphs (String name)
  {
    Query query = session.createQuery( "from HibGraph where name = :name " );
    query.setString(  "name", name );
    return query.list();
  }

  @SuppressWarnings("unchecked")
  @Override
  public List<HibGraph> getGraphs ()
  {
    return session.createQuery( "from HibGraph" ).list();
  }

  @Override
  public HibGraph getLatestGraph (String name)
  {
    String query = "from HibGraph " +
                   " where name = :name " +
                   "   and version in (select max(version) from HibGraph where name = :name)";

    return
       (HibGraph)session.createQuery( query )
       .setString(  "name", name )
       .uniqueResult();
  }

  public HibGraph loadGraph (long graphId)
  {
    return (HibGraph)session.load( HibGraph.class, graphId );
  }

  public HibGraph findGraph (long graphId)
  {
    return (HibGraph)session.get( HibGraph.class, graphId );
  }

  public HibGraphProcess loadProcess (long processId)
  {
    return (HibGraphProcess)session.load( HibGraphProcess.class, processId );
  }

  public HibGraphProcess findProcess (long processId)
  {
    return (HibGraphProcess)session.get( HibGraphProcess.class, processId );
  }

  public HibNodeToken loadNodeToken (long tokenId)
  {
    return (HibNodeToken)session.load( HibNodeToken.class, tokenId );
  }

  public HibNodeToken findNodeToken (long tokenId)
  {
    return (HibNodeToken)session.get( HibNodeToken.class, tokenId );
  }
}