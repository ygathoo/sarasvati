package org.codemonk.wf.test;

import java.io.Console;
import java.util.List;

import org.codemonk.wf.IArc;
import org.codemonk.wf.db.Arc;
import org.codemonk.wf.db.ArcToken;
import org.codemonk.wf.db.Graph;
import org.codemonk.wf.db.HibernateEngine;
import org.codemonk.wf.db.Node;
import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.db.NodeToken;
import org.codemonk.wf.db.Process;
import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.cfg.AnnotationConfiguration;
import org.hibernate.dialect.PostgreSQLDialect;
import org.postgresql.Driver;

public class DbConsole
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

  public static void main (String[] args) throws Exception
  {
    init();

    while ( true )
    {
      Session session = sessionFactory.openSession();
      HibernateEngine engine = new HibernateEngine( session );

      Graph graph = getGraph( engine );
      Process process = (Process)engine.startWorkflow( graph );
      session.flush();
      session.close();

      runWorkflow( process.getId() );
    }
  }

  @SuppressWarnings("unchecked")
  public static void runWorkflow (long processId)
  {
    while (true)
    {
      Session session = sessionFactory.openSession();
      HibernateEngine engine = new HibernateEngine( session );

      Process p = (Process) session.load( Process.class, processId );
      if (p.isComplete() )
      {
        System.out.println( "Workflow complete" );
        return;
      }

      List<Task> tasks =
        session
          .createQuery( "from Task where nodeToken.process = ? order by state" )
          .setEntity( 0, p )
          .list();

      Task t = null;

      while ( t == null )
      {
        int count = 0;
        for (Task task : tasks )
        {
          System.out.println( (++count) + ": " + task.getName() + " - " + task.getState().getDescription() );
        }

        System.out.print( "> " );
        Console c = System.console();
        String input = c.readLine();

        try
        {
          int line = Integer.parseInt( input );
          if ( line > 0 && line <= tasks.size() )
          {
            t = tasks.get( line - 1);
            processTask( t, engine );
          }
          else
          {
            System.out.println( "Please enter a valid number" );
          }
        }
        catch( NumberFormatException nfe )
        {
          System.out.println( "Please enter a valid number" );
        }
      }

      session.flush();
      session.close();
    }
  }

  public static void processTask (Task t, HibernateEngine engine)
  {
    System.out.println( "Task " );
    System.out.println( "\tName        : "  + t.getName() );
    System.out.println( "\tDescription : "  + t.getDescription() );
    System.out.println( "\tState       : "  + t.getState().getDescription() );

    if ( t.getState().getId() != 0 )
    {
      return;
    }

    System.out.println( "1. Complete" );

    if ( t.isRejectable() )
    {
      System.out.println( "2. Reject" );
    }

    System.out.println( "Anything else to cancel" );

    System.out.print( "> " );
    Console c = System.console();
    String input = c.readLine();

    try
    {
      int line = Integer.parseInt( input );
      if ( line == 1 )
      {
        System.out.println( "Completing task" );
        t.setState( (TaskState) engine.getSession().load( TaskState.class, 1 ) );
        engine.completeExecuteNode( t.getNodeToken().getProcess(), t.getNodeToken(), IArc.DEFAULT_ARC );
      }
      else if ( line == 2 && t.isRejectable() )
      {
        System.out.println( "Rejecting task" );
        t.setState( (TaskState) engine.getSession().load( TaskState.class, 2 ) );
        engine.completeExecuteNode( t.getNodeToken().getProcess(), t.getNodeToken(), "reject" );
      }
      else
      {
        System.out.println( "Ok. Doing nothing" );
      }
    }
    catch( NumberFormatException nfe )
    {
      System.out.println( "Ok. Doing nothing" );
    }
  }

  public static Graph getGraph (HibernateEngine engine)
  {
    Graph graph = null;

    while ( graph == null )
    {
      List<Graph> graphs = engine.getGraphs();

      int count = 0;
      for (Graph g : engine.getGraphs() )
      {
        System.out.println( (++count) + ": " + g.getName() + ": version " + g.getVersion() );
      }

      System.out.print( "> " );
      Console c = System.console();
      String input = c.readLine();

      try
      {
        int line = Integer.parseInt( input );
        if ( line > 0 && line <= graphs.size() )
        {
          graph = graphs.get( line - 1);
        }
        else
        {
          System.out.println( "Please enter a valid number" );
        }
      }
      catch( NumberFormatException nfe )
      {
        System.out.println( "Please enter a valid number" );
      }
    }

    return graph;
  }
}