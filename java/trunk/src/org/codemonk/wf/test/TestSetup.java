package org.codemonk.wf.test;

import org.codemonk.wf.db.Arc;
import org.codemonk.wf.db.ArcToken;
import org.codemonk.wf.db.Graph;
import org.codemonk.wf.db.Node;
import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.db.NodeToken;
import org.codemonk.wf.db.Process;
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

    config.addAnnotatedClass( Arc.class );
    config.addAnnotatedClass( ArcToken.class );
    config.addAnnotatedClass( Graph.class );
    config.addAnnotatedClass( Node.class );
    config.addAnnotatedClass( NodeRef.class );
    config.addAnnotatedClass( NodeToken.class );
    config.addAnnotatedClass( Process.class );

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
