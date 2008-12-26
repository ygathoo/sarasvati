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
package com.googlecode.sarasvati;

import java.util.Iterator;

import org.junit.Test;

public class BacktrackSplitJoinTest extends ExecutionTest
{
  @Test public void testJoin() throws Exception
  {
    Graph g = ensureLoaded( "backtrack-split-join" );
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
      "  (C F 3)" +
      "[2 nodeB I F]" +
      "[3 nodeC I F]";
    TestProcess.validate( p, state );

    engine.completeExecution( tokenB, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (C F 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC I F]" +
      "[4 nodeD I F]";
    TestProcess.validate( p, state );

    iter = p.getActiveNodeTokens().iterator();
    NodeToken tokenC = iter.next();
    NodeToken tokenD = iter.next();

    if ( "nodeD".equals( tokenC.getNode().getName() ) )
    {
      NodeToken tmp = tokenC;
      tokenC = tokenD;
      tokenD = tmp;
    }

    engine.completeExecution( tokenC, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (C F 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC C F]" +
      "  (I F nodeE)" +
      "[4 nodeD I F]";
    TestProcess.validate( p, state );

    engine.completeExecution( tokenD, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (C F 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC C F]" +
      "  (C F 5)" +
      "[4 nodeD C F]" +
      "  (C F 5)" +
      "[5 nodeE I F]";

    TestProcess.validate( p, state );

    NodeToken tokenE = p.getActiveNodeTokens().iterator().next();
    engine.completeExecution( tokenE, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (C F 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC C F]" +
      "  (C F 5)" +
      "[4 nodeD C F]" +
      "  (C F 5)" +
      "[5 nodeE C F]" +
      "  (C F 6)" +
      "  (C F 7)" +
      "[6 nodeF I F]" +
      "[7 nodeG I F]";

    TestProcess.validate( p, state );

    iter = p.getActiveNodeTokens().iterator();
    NodeToken tokenF = iter.next();
    NodeToken tokenG = iter.next();

    if ( "nodeF".equals( tokenG.getNode().getName() ) )
    {
      NodeToken tmp = tokenF;
      tokenF = tokenG;
      tokenG = tmp;
    }

    engine.completeExecution( tokenF, Arc.DEFAULT_ARC );

    state =
      "[1 nodeA C F]" +
      "  (C F 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC C F]" +
      "  (C F 5)" +
      "[4 nodeD C F]" +
      "  (C F 5)" +
      "[5 nodeE C F]" +
      "  (C F 6)" +
      "  (C F 7)" +
      "[6 nodeF C F]" +
      "[7 nodeG I F]";

    TestProcess.validate( p, state );

    engine.backtrack( tokenC );

    state =
      "[1 nodeA C F]" +
      "  (C F 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC C FB]" +
      "  (C FB 5)" +
      "[4 nodeD C F]" +
      "  (C FB 5)" +
      "[5 nodeE C FB]" +
      "  (C FB 6)" +
      "  (C FB 7)" +
      "[6 nodeF C FB]" +
      "  (C BB 8)" +
      "[7 nodeG C FB]" +
      "  (C BB 8)" +
      "[8 nodeE C BB]" +
      "  (C BB 9)" +
      "  (I U nodeE)" +
      "[9 nodeC I F]";

    TestProcess.validate( p, state );

    engine.backtrack( tokenA );

    state =
      "[1 nodeA C FB]" +
      "  (C FB 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC C FB]" +
      "  (C FB 5)" +
      "[4 nodeD C F]" +
      "  (C FB 5)" +
      "[5 nodeE C FB]" +
      "  (C FB 6)" +
      "  (C FB 7)" +
      "[6 nodeF C FB]" +
      "  (C BB 8)" +
      "[7 nodeG C FB]" +
      "  (C BB 8)" +
      "[8 nodeE C BB]" +
      "  (C BB 9)" +
      "  (I U nodeE)" +
      "[9 nodeC C FB]" +
      "  (C BB 10)" +
      "[10 nodeA I F]";

    TestProcess.validate( p, state );

    engine.backtrack( tokenD );

    state =
      "[1 nodeA C FB]" +
      "  (C FB 3)" +
      "[2 nodeB C F]" +
      "  (C F 4)" +
      "[3 nodeC C FB]" +
      "  (C FB 5)" +
      "[4 nodeD C FB]" +
      "  (C FB 5)" +
      "[5 nodeE C FB]" +
      "  (C FB 6)" +
      "  (C FB 7)" +
      "[6 nodeF C FB]" +
      "  (C BB 8)" +
      "[7 nodeG C FB]" +
      "  (C BB 8)" +
      "[8 nodeE C BB]" +
      "  (C BB 9)" +
      "  (C BB 11)" +
      "[9 nodeC C FB]" +
      "  (C BB 10)" +
      "[10 nodeA I F]" +
      "[11 nodeD I F]";

    TestProcess.validate( p, state );

    engine.backtrack( tokenB );

    state =
      "[1 nodeA C FB]" +
      "  (C FB 3)" +
      "[2 nodeB C FB]" +
      "  (C FB 4)" +
      "[3 nodeC C FB]" +
      "  (C FB 5)" +
      "[4 nodeD C FB]" +
      "  (C FB 5)" +
      "[5 nodeE C FB]" +
      "  (C FB 6)" +
      "  (C FB 7)" +
      "[6 nodeF C FB]" +
      "  (C BB 8)" +
      "[7 nodeG C FB]" +
      "  (C BB 8)" +
      "[8 nodeE C BB]" +
      "  (C BB 9)" +
      "  (C BB 11)" +
      "[9 nodeC C FB]" +
      "  (C BB 10)" +
      "[10 nodeA I F]" +
      "[11 nodeD C FB]" +
      "  (C BB 12)" +
      "[12 nodeB I F]";

    TestProcess.validate( p, state );
  }
}