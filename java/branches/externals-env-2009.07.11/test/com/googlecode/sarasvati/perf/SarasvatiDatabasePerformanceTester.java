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

    Copyright 2009 Paul Lorenz
*/

package com.googlecode.sarasvati.perf;

import java.io.File;
import java.io.FilenameFilter;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.hibernate.Session;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.example.ApprovalNode;
import com.googlecode.sarasvati.example.ApprovalSetupNode;
import com.googlecode.sarasvati.example.CustomTestNode;
import com.googlecode.sarasvati.example.MessageNode;
import com.googlecode.sarasvati.example.hib.AsyncNode;
import com.googlecode.sarasvati.example.hib.DumpNode;
import com.googlecode.sarasvati.example.hib.HibExampleTaskNode;
import com.googlecode.sarasvati.example.hib.HibTestSetup;
import com.googlecode.sarasvati.example.hib.InitNode;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.hib.HibGraphProcess;
import com.googlecode.sarasvati.hib.HibNode;
import com.googlecode.sarasvati.impl.NestedProcessNode;
import com.googlecode.sarasvati.impl.ScriptNode;
import com.googlecode.sarasvati.impl.WaitNode;
import com.googlecode.sarasvati.load.ProcessDefinitionResolver;
import com.googlecode.sarasvati.load.definition.ProcessDefinition;
import com.googlecode.sarasvati.rubric.env.DefaultRubricFunctionRepository;
import com.googlecode.sarasvati.rubric.env.RubricPredicate;
import com.googlecode.sarasvati.xml.DefaultFileXmlProcessDefinitionResolver;
import com.googlecode.sarasvati.xml.XmlLoader;

public class SarasvatiDatabasePerformanceTester
{
  protected static final Set<String> allowed = new HashSet<String>();

  static
  {
    allowed.add( "densest.wf.xml" );
    allowed.add( "embedded-task-rej.wf.xml" );
    allowed.add( "random-guard.wf.xml" );
    allowed.add( "three-deep.wf.xml" );
    allowed.add( "token-set-approvals.wf.xml" );
    allowed.add( "custom-node.wf.xml" );
  }

  private static final FilenameFilter filter = new FilenameFilter()
  {
    @Override
    public boolean accept( File dir, String name )
    {
      return allowed.contains( name );
    }
  };

  private final Map<String, TestPerfStats> graphs = new HashMap<String,TestPerfStats>();

  private HibEngine newEngine () throws Exception
  {
    Session sess = HibTestSetup.openSession();
    sess.beginTransaction();

    HibEngine engine = new HibEngine( sess );

    engine.addNodeType( "node", HibNode.class);
    engine.addNodeType( "task", HibExampleTaskNode.class );
    engine.addNodeType( "init", InitNode.class );
    engine.addNodeType( "dump", DumpNode.class );
    engine.addNodeType( "async", AsyncNode.class );
    engine.addNodeType( "custom", CustomNode.class );
    engine.addNodeType( "script", ScriptNode.class );
    engine.addNodeType( "nested", NestedProcessNode.class );
    engine.addNodeType( "wait", WaitNode.class );
    engine.addNodeType( "dumpTypeDupe", DumpNode.class );
    engine.addNodeType( "customTest", CustomTestNode.class );
    engine.addNodeType( "approval", ApprovalNode.class );
    engine.addNodeType( "approvalSetup", ApprovalSetupNode.class );
    engine.addNodeType( "message", MessageNode.class );

    return engine;
  }

  public long testLoad (final String name) throws Exception
  {
    HibEngine engine = newEngine();
    long start = System.currentTimeMillis();
    Graph g = engine.getRepository().getLatestGraph( name );
    for ( Node node : g.getNodes() )
    {
      node.getName();
    }
    for ( Arc arc : g.getArcs() )
    {
      arc.getName();
    }
    engine.getSession().close();
    return System.currentTimeMillis() - start;
  }

  public void init () throws Exception
  {
    HibEngine engine = newEngine();
    XmlLoader xmlLoader = new XmlLoader();

    File baseDir = new File( "common/test-wf/" );
    assert baseDir.exists() : "Workflow process def dir not found.";

    ProcessDefinitionResolver resolver = new DefaultFileXmlProcessDefinitionResolver( xmlLoader, baseDir );

    for ( File file : baseDir.listFiles( filter ) )
    {
      ProcessDefinition xmlDef = xmlLoader.translate( file );
      engine.getLoader().loadWithDependencies( xmlDef.getName(), resolver );
      graphs.put( xmlDef.getName(), new TestPerfStats( xmlDef ) );
    }

    engine.getSession().getTransaction().commit();
    engine.getSession().close();
  }

  public void runGraphTest (final int iterations,
                            final int avgRollover)
    throws Exception
  {
    TestPerfStats.setRollover( avgRollover );

    for ( int i = 0; i < iterations; i++ )
    {
      HibEngine engine = newEngine();
      for ( TestPerfStats tg : graphs.values() )
      {
        long start = System.currentTimeMillis();
        engine.getLoader().loadDefinition( tg.getXmlProcDef() );
        tg.addInsertStat( System.currentTimeMillis() - start );
      }
      engine.getSession().getTransaction().commit();
      engine.getSession().close();

      for ( TestPerfStats tg : graphs.values() )
      {
        tg.addLoadStat( testLoad( tg.getName() ) );
      }

      System.out.println( "Completed iteration: " + i );
    }
  }

  public void dumpStats ()
  {
    for ( TestPerfStats tg : graphs.values() )
    {
      tg.dumpStats();
      System.out.println();
    }
  }

  public void testProcess (final String graphName,
                           final int iterations)
    throws Exception
  {
    for ( int count = 0; count < iterations; count++ )
    {
      HibEngine engine = newEngine();
      HibGraphProcess p = (HibGraphProcess) engine.startProcess( graphName );

      long processId = p.getId();

      engine.getSession().getTransaction().commit();
      engine.getSession().close();

      while ( !p.isComplete() )
      {
        int iter = 0;
        engine = newEngine();

        long start = System.currentTimeMillis();
        p = engine.getRepository().findProcess( processId );

        while ( !p.getExecutionQueue().isEmpty() && iter < 202)
        {
          engine.executeQueuedArcTokens( p );
          iter++;
        }
        engine.getSession().getTransaction().commit();
        engine.getSession().close();
        System.out.println( "Iteration " + count + ". Execute time: " + (System.currentTimeMillis() - start ) );
      }
    }
  }

  public static void main (final String[] args) throws Exception
  {
    HibTestSetup.init( false );

    SarasvatiDatabasePerformanceTester perfTester = new SarasvatiDatabasePerformanceTester();
    perfTester.init();

    DefaultRubricFunctionRepository repository = DefaultRubricFunctionRepository.getGlobalInstance();

    repository.registerPredicate( "isRandOdd", new RubricPredicate()
    {
      @Override
      public boolean eval( Engine engine, NodeToken token )
      {
        return token.getEnv().getAttribute( "rand", Long.class ) % 2 == 1;
      }
    });

    repository.registerPredicate( "isRandEven", new RubricPredicate()
    {
      @Override
      public boolean eval( Engine engine, NodeToken token )
      {
        return token.getEnv().getAttribute( "rand", Long.class ) % 2 == 0;
      }
    });

    repository.registerPredicate( "isTenthIteration", new RubricPredicate()
    {
      @Override
      public boolean eval( Engine engine, NodeToken token )
      {
        return token.getEnv().getAttribute( "iter", Long.class ) == 100;
      }
    });

    repository.registerPredicate( "Approved", new RubricPredicate()
    {
      @Override
      public boolean eval( Engine engine, NodeToken token )
      {
        return true;
      }
    });


    DumpNode.doPrint = false;

    System.out.println( "================================START========================================" );
    perfTester.testProcess( "random-guard", 1000 );
    //perfTester.runGraphTest( 100, 10 );
    //perfTester.dumpStats();
  }
}