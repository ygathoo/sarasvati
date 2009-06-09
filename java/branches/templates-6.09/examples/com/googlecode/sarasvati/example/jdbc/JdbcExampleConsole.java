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
package com.googlecode.sarasvati.example.jdbc;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.example.CustomTestNode;
import com.googlecode.sarasvati.example.LoggingExecutionListener;
import com.googlecode.sarasvati.example.TaskState;
import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.JdbcGraphProcess;
import com.googlecode.sarasvati.jdbc.dialect.DatabaseDialect;
import com.googlecode.sarasvati.jdbc.dialect.PostgreSQLDatabaseDialect;
import com.googlecode.sarasvati.load.DefaultNodeFactory;
import com.googlecode.sarasvati.rubric.env.DefaultRubricFunctionRepository;
import com.googlecode.sarasvati.rubric.env.RubricPredicate;

public class JdbcExampleConsole
{
  public static boolean log = false;

  private static final ExampleActionFactory exampleDB = new BaseExampleActionFactory();
  private static final DatabaseDialect dialect = new PostgreSQLDatabaseDialect();

  static
  {
    dialect.setUserData( ExampleActionFactory.class, exampleDB );
  }

  public static void main (String[] args) throws Exception
  {
    DefaultRubricFunctionRepository repository = DefaultRubricFunctionRepository.getGlobalInstance();

    repository.registerPredicate( "isRandOdd", new RubricPredicate()
    {
      @Override
      public boolean eval( Engine engine, NodeToken token )
      {
        return token.getEnv().getLongAttribute( "rand" ) % 2 == 1;
      }
    });

    repository.registerPredicate( "isRandEven", new RubricPredicate()
    {
      @Override
      public boolean eval( Engine engine, NodeToken token )
      {
        return token.getEnv().getLongAttribute( "rand" ) % 2 == 0;
      }
    });

    repository.registerPredicate( "isTenthIteration", new RubricPredicate()
    {
      @Override
      public boolean eval( Engine engine, NodeToken token )
      {
        return token.getEnv().getLongAttribute( "iter" ) == 10;
      }
    });

    DefaultNodeFactory.addGlobalCustomType( "customTest", CustomTestNode.class );

    while ( true )
    {
      Connection conn = JdbcTestSetup.openConnection();
      conn.setAutoCommit( false );

      JdbcEngine engine = new JdbcEngine( conn, dialect );

      JdbcGraph graph = getGraph( engine );

      JdbcGraphProcess process = (JdbcGraphProcess)engine.startProcess( graph );
      conn.commit();

      runWorkflow( process.getId() );
    }
  }

  public static void runWorkflow (long processId) throws SQLException
  {
    while (true)
    {
      Connection conn = JdbcTestSetup.openConnection();
      conn.setAutoCommit( false );
      JdbcEngine engine = new JdbcEngine( conn, dialect );

      JdbcGraphProcess p = engine.getRepository().loadProcess( processId );

      while ( !p.getExecutionQueue().isEmpty() )
      {
        engine.executeQueuedArcTokens( p );
      }

      if ( p.isComplete() )
      {
        System.out.println( "Workflow complete" );
        return;
      }

      List<JdbcExampleTask> tasks = exampleDB.loadTasksForProcess( engine, p );

      JdbcExampleTask t = null;

      while ( t == null )
      {
        int count = 0;
        for (JdbcExampleTask task : tasks )
        {
          System.out.println( (++count) + ": " + task.getName() + " - " + task.getState() );
        }

        System.out.print( "> " );

        String input = readLine();

        try
        {
          if ( "log".equals( input ) )
          {
            if ( p.getListeners().isEmpty() )
            {
              engine.addExecutionListener( p, new LoggingExecutionListener(), ExecutionEventType.values() );
            }
            else
            {
              engine.removeExecutionListener( p, new LoggingExecutionListener(), ExecutionEventType.values() );
            }
            break;
          }
          else
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
        }
        catch( NumberFormatException nfe )
        {
          System.out.println( "Please enter a valid number" );
        }
      }

      conn.commit();
    }
  }

  public static void processTask (JdbcExampleTask t, JdbcEngine engine)
  {
    System.out.println( "Task " );
    System.out.println( "\tName        : "  + t.getName() );
    System.out.println( "\tDescription : "  + t.getDescription() );
    System.out.println( "\tState       : "  + t.getState() );

    boolean backtrackable = t.getNodeToken().isComplete() && !t.getNodeToken().getExecutionType().isBacktracked();

    if ( t.getState().getId() != 0 && !backtrackable )
    {
      return;
    }

    if ( backtrackable )
    {
      System.out.println( "1. Backtrack" );
    }
    else
    {
      System.out.println( "1. Complete" );

      if ( t.isRejectable() )
      {
        System.out.println( "2. Reject" );
      }
    }

    System.out.println( "Anything else to cancel" );

    System.out.print( "> " );
    String input = readLine();

    try
    {
      int line = Integer.parseInt( input );
      if ( line == 1 )
      {
        if ( backtrackable )
        {
          System.out.println( "Backtracking to task" );
          engine.backtrack( t.getNodeToken() );
        }
        else
        {
          System.out.println( "Completing task" );
          t.setState( TaskState.Completed );
          exampleDB.newUpdateTaskAction( t ).execute( engine );
          engine.completeExecution( t.getNodeToken(), Arc.DEFAULT_ARC );
        }
      }
      else if ( line == 2 && t.isRejectable() )
      {
        System.out.println( "Rejecting task" );
        t.setState( TaskState.Rejected );
        exampleDB.newUpdateTaskAction( t ).execute( engine );
        engine.completeExecution( t.getNodeToken(), "reject" );
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

  public static JdbcGraph getGraph (JdbcEngine engine)
  {
    JdbcGraph graph = null;

    while ( graph == null )
    {
      List<JdbcGraph> graphs = engine.getRepository().getGraphs();

      int count = 0;
      for ( JdbcGraph g : graphs )
      {
        System.out.println( (++count) + ": " + g.getName() + ": version " + g.getVersion() );
      }

      System.out.print( "> " );
      String input = readLine();

      if ( "log".equals( input ) )
      {
        log = !log;
        if ( log )
        {
          engine.addExecutionListener( new LoggingExecutionListener(), ExecutionEventType.values() );
        }
        else
        {
          engine.removeExecutionListener( new LoggingExecutionListener() );
        }
        System.out.println( "Logging set to: " + log );
        continue;
      }

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

  public static String readLine ()
  {
    try
    {
      return new BufferedReader( new InputStreamReader( System.in ) ).readLine();
    }
    catch (IOException ioe )
    {
      throw new RuntimeException( ioe );
    }
  }
}