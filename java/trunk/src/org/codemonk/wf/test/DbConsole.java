package org.codemonk.wf.test;

import java.io.Console;
import java.util.List;

import org.codemonk.wf.IArc;
import org.codemonk.wf.db.HibGraph;
import org.codemonk.wf.db.HibEngine;
import org.codemonk.wf.db.HibProcess;
import org.hibernate.Session;
import org.hibernate.Transaction;

public class DbConsole
{
  public static void main (String[] args) throws Exception
  {
    TestSetup.init();

    while ( true )
    {
      Session session = TestSetup.openSession();
      Transaction t = session.beginTransaction();
      HibEngine engine = new HibEngine( session );

      HibGraph graph = getGraph( engine );
      HibProcess process = (HibProcess)engine.startWorkflow( graph );
      session.flush();
      t.commit();
      session.close();

      runWorkflow( process.getId() );
    }
  }

  @SuppressWarnings("unchecked")
  public static void runWorkflow (long processId)
  {
    while (true)
    {
      Session session = TestSetup.openSession();
      Transaction trans = session.beginTransaction();
      HibEngine engine = new HibEngine( session );

      HibProcess p = (HibProcess) session.load( HibProcess.class, processId );
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
      trans.commit();
      session.close();
    }
  }

  public static void processTask (Task t, HibEngine engine)
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

  public static HibGraph getGraph (HibEngine engine)
  {
    HibGraph graph = null;

    while ( graph == null )
    {
      List<HibGraph> graphs = engine.getGraphs();

      int count = 0;
      for (HibGraph g : engine.getGraphs() )
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