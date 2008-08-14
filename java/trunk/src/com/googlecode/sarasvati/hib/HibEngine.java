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

import org.hibernate.Session;
import org.hibernate.cfg.AnnotationConfiguration;

import com.googlecode.sarasvati.NonRecursiveEngine;

public class HibEngine extends NonRecursiveEngine
{
  protected Session session;
  protected HibGraphFactory factory;
  protected HibGraphRepository repository;

  public HibEngine (Session session)
  {
    this.session = session;
    this.factory = new HibGraphFactory( session );
    this.repository = new HibGraphRepository( session );
  }

  public Session getSession ()
  {
    return session;
  }

  public void setSession (Session session)
  {
    this.session = session;
  }

  @Override
  public HibGraphRepository getRepository()
  {
    return repository;
  }

  @Override
  public HibGraphFactory getFactory()
  {
    return factory;
  }

  public static void addToConfiguration (AnnotationConfiguration config, boolean enableCaching)
  {
    config.addAnnotatedClass( HibArc.class );
    config.addAnnotatedClass( HibArcToken.class );
    config.addAnnotatedClass( HibGraph.class );
    config.addAnnotatedClass( HibNode.class );
    config.addAnnotatedClass( HibNodeRef.class );
    config.addAnnotatedClass( HibNodeToken.class );
    config.addAnnotatedClass( HibProcess.class );

    if (enableCaching )
    {
      config.setCacheConcurrencyStrategy( HibGraph.class.getName(),"read-only" );
      config.setCacheConcurrencyStrategy( HibNode.class.getName(),"read-only" );
      config.setCacheConcurrencyStrategy( HibNodeRef.class.getName(),"read-only" );
      config.setCacheConcurrencyStrategy( HibArc.class.getName(),"read-only" );
    }
  }
}