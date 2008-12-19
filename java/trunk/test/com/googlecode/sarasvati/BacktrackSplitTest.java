package com.googlecode.sarasvati;

import org.junit.Test;

public class BacktrackSplitTest extends BacktrackTest
{
  @Test public void testSplit() throws Exception
  {
    Graph g = ensureLoaded( "backtrack-split" );
    GraphProcess p = engine.startProcess( g );

    NodeToken tokenA = p.getActiveNodeTokens().iterator().next();

    String state = "[1 nodeA I F]";
    TestProcess.validate( p, state );

    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (C F 2)" +
      "  (C F 3)" +
      "  (C F 4)" +
      "[2 nodeB I F]" +
      "[3 nodeC I F]" +
      "[4 nodeD I F]"
      ;
    TestProcess.validate( p, state );

    engine.backtrack( tokenA );

    state =
      "[1 nodeA C FB]" +
      "  (C FB 2)" +
      "  (C FB 3)" +
      "  (C FB 4)" +
      "[2 nodeB C FB]" +
      "  (C BB 5)" +
      "[3 nodeC C FB]" +
      "  (C BB 5)" +
      "[4 nodeD C FB]" +
      "  (C BB 5)" +
      "[5 nodeA I F]"
      ;

    TestProcess.validate( p, state );

    tokenA = p.getActiveNodeTokens().iterator().next();
    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );
    state =
      "[1 nodeA C FB]" +
      "  (C FB 2)" +
      "  (C FB 3)" +
      "  (C FB 4)" +
      "[2 nodeB C FB]" +
      "  (C BB 5)" +
      "[3 nodeC C FB]" +
      "  (C BB 5)" +
      "[4 nodeD C FB]" +
      "  (C BB 5)" +
      "[5 nodeA C F]" +
      "  (C F 6)" +
      "  (C F 7)" +
      "  (C F 8)" +
      "[6 nodeB I F]" +
      "[7 nodeC I F]" +
      "[8 nodeD I F]"
      ;

    TestProcess.validate( p, state );

    engine.backtrack( tokenA );

    state =
      "[1 nodeA C FB]" +
      "  (C FB 2)" +
      "  (C FB 3)" +
      "  (C FB 4)" +
      "[2 nodeB C FB]" +
      "  (C BB 5)" +
      "[3 nodeC C FB]" +
      "  (C BB 5)" +
      "[4 nodeD C FB]" +
      "  (C BB 5)" +
      "[5 nodeA C FB]" +
      "  (C FB 6)" +
      "  (C FB 7)" +
      "  (C FB 8)" +
      "[6 nodeB C FB]" +
      "  (C BB 9)" +
      "[7 nodeC C FB]" +
      "  (C BB 9)" +
      "[8 nodeD C FB]" +
      "  (C BB 9)" +
      "[9 nodeA I F]"
      ;

    TestProcess.validate( p, state );
  }
}