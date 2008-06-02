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
package org.codemonk.wf.test;

import java.io.Console;
import java.util.List;

import org.codemonk.wf.Arc;
import org.codemonk.wf.WfEngine;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.guardlang.GuardLangPredicate;
import org.codemonk.wf.guardlang.PredicateRepository;
import org.codemonk.wf.hib.HibWfGraph;
import org.codemonk.wf.hib.HibWfEngine;
import org.codemonk.wf.hib.HibProcess;
import org.hibernate.Session;
import org.hibernate.Transaction;

public class DbConsole
{
  public static void main (String[] args) throws Exception
  {
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


    TestSetup.init();

    while ( true )
    {
      Session session = TestSetup.openSession();
      Transaction t = session.beginTransaction();
      HibWfEngine engine = new HibWfEngine( session );

      HibWfGraph graph = getGraph( engine );
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
      HibWfEngine engine = new HibWfEngine( session );

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

  public static void processTask (Task t, HibWfEngine engine)
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
        engine.completeExecuteNode( t.getNodeToken(), Arc.DEFAULT_ARC );
      }
      else if ( line == 2 && t.isRejectable() )
      {
        System.out.println( "Rejecting task" );
        t.setState( (TaskState) engine.getSession().load( TaskState.class, 2 ) );
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

  public static HibWfGraph getGraph (HibWfEngine engine)
  {
    HibWfGraph graph = null;

    while ( graph == null )
    {
      List<HibWfGraph> graphs = engine.getGraphs();

      int count = 0;
      for ( HibWfGraph g : engine.getGraphs() )
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