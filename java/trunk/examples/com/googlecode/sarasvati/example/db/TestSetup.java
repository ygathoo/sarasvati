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

package com.googlecode.sarasvati.example.db;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.dialect.PostgreSQLDialect;
import org.postgresql.Driver;

import com.googlecode.sarasvati.hib.HibArc;
import com.googlecode.sarasvati.hib.HibArcToken;
import com.googlecode.sarasvati.hib.HibNode;
import com.googlecode.sarasvati.hib.HibNodeRef;
import com.googlecode.sarasvati.hib.HibNodeToken;
import com.googlecode.sarasvati.hib.HibProcess;
import com.googlecode.sarasvati.hib.HibGraph;

public class TestSetup
{
  protected static SessionFactory sessionFactory = null;

  public static void init () throws Exception
  {
    AnnotationConfiguration config = new AnnotationConfiguration ();
    config.setProperty( "hibernate.dialect", PostgreSQLDialect.class.getName() );
    config.setProperty( "hibernate.connection.driver_class", Driver.class.getName() );
    config.setProperty( "hibernate.connection.url", "jdbc:postgresql://localhost:5433/paul" );
    config.setProperty( "hibernate.connection.username", "paul" );
    config.setProperty( "hibernate.connection.password", "thesistest56" );
    config.setProperty( "hibernate.query.substitutions", "true=Y, false=N" );
    //config.setProperty( "hibernate.show_sql", "true" );
    //config.setProperty( "hibernate.format_sql", "true" );

    config.addAnnotatedClass( HibArc.class );
    config.addAnnotatedClass( HibArcToken.class );
    config.addAnnotatedClass( HibGraph.class );
    config.addAnnotatedClass( HibNode.class );
    config.addAnnotatedClass( HibNodeRef.class );
    config.addAnnotatedClass( HibNodeToken.class );
    config.addAnnotatedClass( HibProcess.class );

    config.addAnnotatedClass( NodeTask.class );
    config.addAnnotatedClass( Task.class );
    config.addAnnotatedClass( TaskState.class );
    config.addAnnotatedClass( NodeTaskDetail.class );
    config.addAnnotatedClass( NodeInit.class );
    config.addAnnotatedClass( NodeDump.class );

    sessionFactory = config.buildSessionFactory();
  }

  public static Session openSession ()
  {
    return sessionFactory.openSession();
  }
}
