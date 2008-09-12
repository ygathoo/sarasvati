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
package com.googlecode.sarasvati.example.mem;

import java.io.BufferedReader;
import java.io.File;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.example.LoggingExecutionListener;
import com.googlecode.sarasvati.guardlang.GuardLangPredicate;
import com.googlecode.sarasvati.guardlang.PredicateRepository;
import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.mem.MemEngine;
import com.googlecode.sarasvati.mem.MemGraph;
import com.googlecode.sarasvati.xml.DefaultFileXmlWorkflowResolver;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlWorkflowResolver;

public class MemConsole
{
  public static boolean log = false;

  public static void main (String[] args) throws Exception
  {
    loadWorkflows();

    PredicateRepository.addPredicate( "isRandOdd", new GuardLangPredicate()
    {
      @Override
      public boolean evaluate( Engine engine, NodeToken token )
      {
        return token.getEnv().getLongAttribute( "rand" ) % 2 == 1;
      }
    });

    PredicateRepository.addPredicate( "isRandEven", new GuardLangPredicate()
    {
      @Override
      public boolean evaluate( Engine engine, NodeToken token )
      {
        return token.getEnv().getLongAttribute( "rand" ) % 2 == 0;
      }
    });

    PredicateRepository.addPredicate( "isTenthIteration", new GuardLangPredicate()
    {
      @Override
      public boolean evaluate( Engine engine, NodeToken token )
      {
        System.out.println( "iter: " + token.getEnv().getLongAttribute( "iter" ) );
        return token.getEnv().getLongAttribute( "iter" ) == 1000;
      }
    });

    while ( true )
    {
      MemEngine engine = new MemEngine();

      MemGraph graph = getGraph();
      GraphProcess process = engine.getFactory().newProcess( graph );

      engine.startProcess( process );

      runWorkflow( process );
    }
  }

  public static void runWorkflow (GraphProcess process)
  {
    while (true)
    {
      MemEngine engine = new MemEngine();
      if ( process.isComplete() )
      {
        System.out.println( "Workflow complete" );
        return;
      }

      List<Task> tasks = TaskList.getTasks();

      Task t = null;

      while ( t == null )
      {
        int count = 0;
        for (Task task : tasks )
        {
          System.out.println( (++count) + ": " + task.getName() + " - " + task.getState() );
        }

        System.out.print( "> " );
        String input = readLine();

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
    }
  }

  public static void processTask (Task t, MemEngine engine)
  {
    System.out.println( "Task " );
    System.out.println( "\tName        : "  + t.getName() );
    System.out.println( "\tDescription : "  + t.getDescription() );
    System.out.println( "\tState       : "  + t.getState() );

    if ( t.getState() != TaskState.Open )
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
    String input = readLine();

    try
    {
      int line = Integer.parseInt( input );
      if ( line == 1 )
      {
        System.out.println( "Completing task" );
        t.setState( TaskState.Completed );
        engine.completeExecution( t.getNodeToken(), Arc.DEFAULT_ARC );
      }
      else if ( line == 2 && t.isRejectable() )
      {
        System.out.println( "Rejecting task" );
        t.setState( TaskState.Rejected );
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

  public static MemGraph getGraph ()
  {
    MemEngine engine = new MemEngine();

    MemGraph graph = null;

    while ( graph == null )
    {
      List<MemGraph> graphs = engine.getRepository().getGraphs();

      int count = 0;
      for ( MemGraph g : graphs )
      {
        System.out.println( (++count) + ": " + g.getName() + ": version " + g.getVersion() );
      }

      System.out.print( "> " );
      String input = readLine();

      if ( "log".equals( input ) )
      {
        if ( log )
        {
          engine.removeExecutionListener( new LoggingExecutionListener() );
        }
        else
        {
          engine.addExecutionListener( new LoggingExecutionListener(), ExecutionEventType.values() );
        }

        log = !log;
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

  public static void loadWorkflows () throws Exception
  {
    XmlLoader xmlLoader = new XmlLoader();
    MemEngine engine = new MemEngine();

    engine.getFactory().addType( "task", TaskNode.class );
    engine.getFactory().addType( "init", InitNode.class );
    engine.getFactory().addType( "dump", DumpNode.class );

    GraphLoader<MemGraph> wfLoader = new GraphLoader<MemGraph>( engine.getFactory(), engine.getRepository() );

    File basePath = new File( "/home/paul/workspace/wf-common/test-wf/" );
    XmlWorkflowResolver resolver = new DefaultFileXmlWorkflowResolver(xmlLoader, basePath );

    FilenameFilter filter = new FilenameFilter()
    {
      @Override
      public boolean accept (File dir, String name)
      {
        return name.endsWith( ".wf.xml" );
      }
    };

    for ( File file : basePath.listFiles( filter ) )
    {
      String name = file.getName();
      name = name.substring( 0, name.length() - ".wf.xml".length() );

      if ( engine.getRepository().getLatestGraph( name ) == null )
      {
        try
        {
          wfLoader.loadWithDependencies( name, resolver );
        }
        catch( Exception e )
        {
          System.out.println( "Failed to load: " + name + "   Message: " + e.getMessage() );
          e.printStackTrace();
        }
      }
    }
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