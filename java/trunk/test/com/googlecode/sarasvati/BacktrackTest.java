package com.googlecode.sarasvati;

import java.io.File;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;

import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.mem.MemEngine;
import com.googlecode.sarasvati.mem.MemGraph;

public class BacktrackTest
{
  protected MemEngine engine;

  @Before
  public void setup ()
  {
    engine = new MemEngine();
  }

  protected Graph ensureLoaded (String name) throws Exception
  {
    File basePath = new File( "/home/paul/workspace/wf-common/unit-test/" );
    GraphLoader<MemGraph> loader = engine.getLoader();

    if ( !loader.isLoaded( name ) )
    {
      loader.load( new File( basePath, name + ".wf.xml" ) );
    }
    return engine.getRepository().getLatestGraph( name );
  }

  @Test public void testLinear () throws Exception
  {
    Graph g = ensureLoaded( "backtrack-linear" );
    GraphProcess p = engine.startProcess( g );
    Collection<? extends NodeToken> tokens = p.getActiveNodeTokens();

    String state = "[1 nodeA I F]";
    TestProcess.validate( p, state );

    NodeToken tokenA = tokens.iterator().next();
    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );

    tokens = p.getActiveNodeTokens();

    state = "[1 nodeA C F]\n" +
            "  (C F 2)\n" +
            "[2 nodeB I F]";
    TestProcess.validate( p, state );

    engine.backtrack( tokenA );

    state = "[1 nodeA C FB]\n" +
            "  (C FB 2)\n" +
            "[2 nodeB C FB]\n" +
            "  (C BB 3)\n" +
            "[3 nodeA I F]\n";
    TestProcess.validate( p, state );
  }

  @Test public void testLinearTwice () throws Exception
  {
    Graph g = ensureLoaded( "backtrack-twice-linear" );
    GraphProcess p = engine.startProcess( g );
    Collection<? extends NodeToken> tokens = p.getActiveNodeTokens();

    String state = "[1 nodeA I F]";
    TestProcess.validate( p, state );

    NodeToken tokenA = tokens.iterator().next();
    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );

    tokens = p.getActiveNodeTokens();

    state = "[1 nodeA C F]\n" +
            "  (C F 2)\n" +
            "[2 nodeB I F]";
    TestProcess.validate( p, state );

    tokens = p.getActiveNodeTokens();
    NodeToken tokenB = tokens.iterator().next();

    engine.completeExecution( tokenB, Arc.DEFAULT_ARC );

    state = "[1 nodeA C F]\n" +
            "  (C F 2)\n" +
            "[2 nodeB C F]\n" +
            "  (C F 3)\n" +
            "[3 nodeC I F]";
    TestProcess.validate( p, state );

    engine.backtrack( tokenB );

    state = "[1 nodeA C F]\n" +
            "  (C F 2)\n" +
            "[2 nodeB C FB]\n" +
            "  (C FB 3)\n" +
            "[3 nodeC C FB]\n" +
            "  (C BB 4)\n" +
            "[4 nodeB I F]\n";

    TestProcess.validate( p, state );
    engine.backtrack( tokenA );

    state = "[1 nodeA C FB]\n" +
            "  (C FB 2)\n" +
            "[2 nodeB C FB]\n" +
            "  (C FB 3)\n" +
            "[3 nodeC C FB]\n" +
            "  (C BB 4)\n" +
            "[4 nodeB C FB]\n" +
            "  (C BB 5)\n" +
            "[5 nodeA I F]\n";

    TestProcess.validate( p, state );
  }
}