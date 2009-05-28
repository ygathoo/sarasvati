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

package com.googlecode.sarasvati.example.hib;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.cfg.Environment;

import com.googlecode.sarasvati.hib.HibEngine;

public class HibTestSetup
{
  protected static SessionFactory sessionFactory = null;

  public static void init (boolean createSchema) throws Exception
  {
    AnnotationConfiguration config = new AnnotationConfiguration ();
    
    /*
     * Postgres Dialect
     */
//    config.setProperty( "hibernate.dialect","org.hibernate.dialect.PostgreSQLDialect" );
//    config.setProperty( "hibernate.connection.driver_class", "org.postgresql.Driver" );
//    config.setProperty( "hibernate.connection.url", "jdbc:postgresql://localhost:5432/paul" );
//    config.setProperty( "hibernate.connection.username", "paul" );
//    config.setProperty( "hibernate.connection.password", "thesistest56" );
//    config.setProperty( "hibernate.query.substitutions", "true=Y, false=N" );
//    config.setProperty( "hibernate.cache.use_second_level_cache", "true" );
    
    /*
     * HSQL Dialect
     */
    config.setProperty( "hibernate.dialect","org.hibernate.dialect.HSQLDialect" );
    config.setProperty( "hibernate.connection.driver_class", "org.hsqldb.jdbcDriver" );
    config.setProperty( "hibernate.connection.url", "jdbc:hsqldb:hsql://localhost/workflowdb;sql.enforce_strict_size=true" );
    config.setProperty( "hibernate.connection.username", "sa" );
    config.setProperty( "hibernate.connection.password", "" );
    config.setProperty( "hibernate.query.substitutions", "true=Y, false=N" );
    config.setProperty( "hibernate.cache.use_second_level_cache", "true" );

    if(createSchema){
      config.setProperty(Environment.HBM2DDL_AUTO, "create-drop");
    }
    
    //config.setProperty( "hibernate.show_sql", "true" );
    //config.setProperty( "hibernate.format_sql", "true" );

    HibEngine.addToConfiguration( config, false );

    config.addAnnotatedClass( HibExampleTaskNode.class );
    config.addAnnotatedClass( Task.class );
    config.addAnnotatedClass( InitNode.class );
    config.addAnnotatedClass( DumpNode.class );
    config.addAnnotatedClass( AsyncNode.class );

    sessionFactory = config.buildSessionFactory();
  }

  public static Session openSession ()
  {
    return sessionFactory.openSession();
  }
}
