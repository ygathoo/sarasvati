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

public class BacktrackJoinTest extends ExecutionTest
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