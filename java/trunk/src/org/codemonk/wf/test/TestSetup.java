package org.codemonk.wf.test;

import org.codemonk.wf.db.HibArc;
import org.codemonk.wf.db.HibArcToken;
import org.codemonk.wf.db.HibGraph;
import org.codemonk.wf.db.HibNode;
import org.codemonk.wf.db.HibNodeRef;
import org.codemonk.wf.db.HibNodeToken;
import org.codemonk.wf.db.HibProcess;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.dialect.PostgreSQLDialect;
import org.postgresql.Driver;

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

    sessionFactory = config.buildSessionFactory();
  }

  public static Session openSession ()
  {
    return sessionFactory.openSession();
  }
}
