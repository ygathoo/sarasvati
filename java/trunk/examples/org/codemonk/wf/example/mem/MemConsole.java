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
package org.codemonk.wf.example.mem;

import java.io.Console;
import java.io.File;
import java.io.FilenameFilter;
import java.util.List;

import org.codemonk.wf.Arc;
import org.codemonk.wf.ImportException;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.WfEngine;
import org.codemonk.wf.guardlang.GuardLangPredicate;
import org.codemonk.wf.guardlang.PredicateRepository;
import org.codemonk.wf.mem.MemNode;
import org.codemonk.wf.mem.MemProcess;
import org.codemonk.wf.mem.MemWfEngine;
import org.codemonk.wf.mem.MemWfGraph;
import org.codemonk.wf.mem.MemWfGraphCache;
import org.codemonk.wf.mem.MemWfLoader;
import org.codemonk.wf.test.XmlTaskDef;
import org.codemonk.wf.xml.DefaultFileXmlWorkflowResolver;
import org.codemonk.wf.xml.XmlLoader;
import org.codemonk.wf.xml.XmlWorkflowResolver;

public class MemConsole
{
  public static void main (String[] args) throws Exception
  {
    loadWorkflows();

    PredicateRepository.addPredicate( "isRandOdd", new GuardLangPredicate()
    {
      @Override
      public boolean evaluate( WfEngine engine, NodeToken token )
      {
        return token.getLongAttribute( "rand" ) % 2 == 1;
      }
    });

    PredicateRepository.addPredicate( "isRandEven", new GuardLangPredicate()
    {
      @Override
      public boolean evaluate( WfEngine engine, NodeToken token )
      {
        return token.getLongAttribute( "rand" ) % 2 == 0;
      }
    });

    PredicateRepository.addPredicate( "isTenthIteration", new GuardLangPredicate()
    {
      @Override
      public boolean evaluate( WfEngine engine, NodeToken token )
      {
        return token.getLongAttribute( "iter" ) == 10;
      }
    });


    while ( true )
    {
      MemWfEngine engine = new MemWfEngine();

      MemWfGraph graph = getGraph( engine );
      MemProcess process = (MemProcess)engine.startWorkflow( graph );

      runWorkflow( process );
    }
  }

  @SuppressWarnings("unchecked")
  public static void runWorkflow (MemProcess process)
  {
    while (true)
    {
      MemWfEngine engine = new MemWfEngine();
      if (process.isComplete() )
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
    }
  }

  public static void processTask (Task t, MemWfEngine engine)
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
    Console c = System.console();
    String input = c.readLine();

    try
    {
      int line = Integer.parseInt( input );
      if ( line == 1 )
      {
        System.out.println( "Completing task" );
        t.setState( TaskState.Completed );
        engine.completeExecuteNode( t.getNodeToken(), Arc.DEFAULT_ARC );
      }
      else if ( line == 2 && t.isRejectable() )
      {
        System.out.println( "Rejecting task" );
        t.setState( TaskState.Rejected );
        engine.completeExecuteNode( t.getNodeToken(), "reject" );
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

  public static MemWfGraph getGraph (MemWfEngine engine)
  {
    MemWfGraph graph = null;

    while ( graph == null )
    {
      List<MemWfGraph> graphs = MemWfGraphCache.getGraphs();

      int count = 0;
      for ( MemWfGraph g : graphs )
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

  public static void loadWorkflows () throws Exception
  {
    XmlLoader xmlLoader = new XmlLoader( XmlTaskDef.class );
    MemWfLoader wfLoader = new MemWfLoader();

    wfLoader.addCustomType( "task", new MemWfLoader.NodeFactory()
    {
      @Override
      public MemNode createNode( MemNode node, Object custom )
        throws ImportException
      {
        if ( custom == null || !(custom instanceof XmlTaskDef) )
        {
          throw new ImportException( "Task node '" + node.getName() +
                                     "' in definition of '" + node.getGraph().getName() +
                                     "' contains no (or improperly specified) task-def element." );
        }

        XmlTaskDef taskDef = (XmlTaskDef)custom;

        NodeTask nodeTask = new NodeTask( node );
        nodeTask.setTaskName( taskDef.getTaskName() );
        nodeTask.setTaskDesc( taskDef.getDescription() );

        return nodeTask;
      }
    });

    wfLoader.addCustomType( "init", new MemWfLoader.NodeFactory()
    {
      @Override
      public MemNode createNode( MemNode node, Object custom )
      {
        return new NodeInit( node );
      }
    });

    wfLoader.addCustomType( "dump", new MemWfLoader.NodeFactory()
    {
      @Override
      public MemNode createNode( MemNode node, Object custom )
      {
        return new NodeDump( node );
      }
    });

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

      if ( MemWfGraphCache.get( name ) == null )
      {
        try
        {
          wfLoader.importWithDependencies( name, resolver );
        }
        catch( Exception e )
        {
          System.out.println( "Failed to load: " + name + "   Message: " + e.getMessage() );
          e.printStackTrace();
        }
      }
    }
  }
}