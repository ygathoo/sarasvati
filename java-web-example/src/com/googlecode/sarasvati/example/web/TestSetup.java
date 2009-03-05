package com.googlecode.sarasvati.example.web;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.dialect.PostgreSQLDialect;
import org.postgresql.Driver;

import com.googlecode.sarasvati.example.db.AsyncNode;
import com.googlecode.sarasvati.example.db.DumpNode;
import com.googlecode.sarasvati.example.db.InitNode;
import com.googlecode.sarasvati.example.db.Task;
import com.googlecode.sarasvati.example.db.TaskNode;
import com.googlecode.sarasvati.example.db.TaskState;
import com.googlecode.sarasvati.hib.HibEngine;

public class TestSetup
{
  public static SessionFactory sessionFactory = null;

  static
  {
    try
    {
      AnnotationConfiguration config = new AnnotationConfiguration ();
      config.setProperty( "hibernate.dialect", PostgreSQLDialect.class.getName() );
      config.setProperty( "hibernate.connection.driver_class", Driver.class.getName() );
      config.setProperty( "hibernate.connection.url", "jdbc:postgresql://localhost:5432/paul" );
      config.setProperty( "hibernate.connection.username", "paul" );
      config.setProperty( "hibernate.connection.password", "thesistest56" );
      config.setProperty( "hibernate.query.substitutions", "true=Y, false=N" );
      config.setProperty( "hibernate.cache.use_second_level_cache", "true" );

      //config.setProperty( "hibernate.show_sql", "true" );
      //config.setProperty( "hibernate.format_sql", "true" );

      HibEngine.addToConfiguration( config, false );

      config.addAnnotatedClass( TaskNode.class );
      config.addAnnotatedClass( Task.class );
      config.addAnnotatedClass( TaskState.class );
      config.addAnnotatedClass( InitNode.class );
      config.addAnnotatedClass( DumpNode.class );
      config.addAnnotatedClass( AsyncNode.class );

      sessionFactory = config.buildSessionFactory();

      System.out.println( "Hibernate initialized" );
    }
    catch (Throwable t)
    {
      System.err.println( t.getMessage() );
      t.printStackTrace();
    }
  }

  public static Session openSession ()
  {
    return sessionFactory.openSession();
  }
}
