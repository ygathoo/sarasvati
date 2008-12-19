package com.googlecode.sarasvati;

import java.util.Iterator;

import org.junit.Test;

public class BacktrackJoinTest extends BacktrackTest
{
  @Test public void testJoin() throws Exception
  {
    Graph g = ensureLoaded( "backtrack-join" );
    GraphProcess p = engine.startProcess( g );

    Iterator<? extends NodeToken> iter = p.getActiveNodeTokens().iterator();
    NodeToken tokenA = iter.next();
    NodeToken tokenB = iter.next();

    if ( "nodeB".equals( tokenA.getNode().getName() ) )
    {
      NodeToken tmp = tokenA;
      tokenA = tokenB;
      tokenB = tmp;
    }

    String state =
      "[1 nodeA I F]" +
      "[2 nodeB I F]";
    TestProcess.validate( p, state );

    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (I F nodeC)" +
      "[2 nodeB I F]";
    TestProcess.validate( p, state );

    engine.completeExecution( tokenB, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (C F 3)" +
      "[2 nodeB C F]" +
      "  (C F 3)" +
      "[3 nodeC I F]";
    TestProcess.validate( p, state );

    engine.backtrack( tokenA );

    state =
      "[1 nodeA C FB]" +
      "  (C FB 3)" +
      "[2 nodeB C F]" +
      "  (C FB 3)" +
      "[3 nodeC C FB]" +
      "  (C BB 4)" +
      "  (I U nodeC)" +
      "[4 nodeA I F]";
    TestProcess.validate( p, state );

    engine.backtrack( tokenB );

    state =
      "[1 nodeA C FB]" +
      "  (C FB 3)" +
      "[2 nodeB C F]" +
      "  (C FB 3)" +
      "[3 nodeC C FB]" +
      "  (C BB 4)" +
      "  (C BB 5)" +
      "[4 nodeA I F]" +
      "[5 nodeB I F]";
    TestProcess.validate( p, state );
  }
}