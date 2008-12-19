package com.googlecode.sarasvati;

import java.util.Collection;

import org.junit.Test;

public class BacktrackLinearTest extends BacktrackTest
{
  @Test public void testLinear () throws Exception
  {
    Graph g = ensureLoaded( "backtrack-linear" );
    GraphProcess p = engine.startProcess( g );
    Collection<? extends NodeToken> tokens = p.getActiveNodeTokens();
    NodeToken tokenA = tokens.iterator().next();

    String state = "[1 nodeA I F]";
    TestProcess.validate( p, state );

    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );

    tokens = p.getActiveNodeTokens();

    state = "[1 nodeA C F]" +
            "  (C F 2)" +
            "[2 nodeB I F]";
    TestProcess.validate( p, state );

    engine.backtrack( tokenA );

    state = "[1 nodeA C FB]" +
            "  (C FB 2)" +
            "[2 nodeB C FB]" +
            "  (C BB 3)" +
            "[3 nodeA I F]";
    TestProcess.validate( p, state );

    tokens = p.getActiveNodeTokens();
    tokenA = tokens.iterator().next();

    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );

    state = "[1 nodeA C FB]" +
            "  (C FB 2)" +
            "[2 nodeB C FB]" +
            "  (C BB 3)" +
            "[3 nodeA C F]" +
            "  (C F 4)" +
            "[4 nodeB I F]"
            ;

    TestProcess.validate( p, state );
    engine.backtrack( tokenA );

    state = "[1 nodeA C FB]" +
            "  (C FB 2)" +
            "[2 nodeB C FB]" +
            "  (C BB 3)" +
            "[3 nodeA C FB]" +
            "  (C FB 4)" +
            "[4 nodeB C FB]" +
            "  (C BB 5)" +
            "[5 nodeA I F]"
            ;
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
