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

    Copyright 2010 Paul Lorenz
*/
package com.googlecode.sarasvati.unittest.env;

import junit.framework.Assert;

import org.junit.Test;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.unittest.framework.ExecutionTest;

public class TestTransientsAttributes extends ExecutionTest
{
  @Test
  public void testSetGetTokenEnv () throws Exception
  {
    Graph graph = ensureLoaded( "two-node" );
    GraphProcess p = engine.startProcess( graph );

    NodeToken t = getActiveToken( p, "nodeA");

    String expected = "bar";
    t.getEnv().setTransientAttribute( "foo", expected );
    String actual = (String)t.getEnv().getTransientAttribute( "foo" );

    Assert.assertEquals( expected, actual );
  }

  @Test
  public void testSetGetTokenFullEnv () throws Exception
  {
    Graph graph = ensureLoaded( "two-node" );
    GraphProcess p = engine.startProcess( graph );

    NodeToken t = getActiveToken( p, "nodeA");

    String expected = "bar";
    t.getFullEnv().setTransientAttribute( "foo", expected );
    String actual = (String)t.getFullEnv().getTransientAttribute( "foo" );

    Assert.assertEquals( expected, actual );
  }

  @Test
  public void testSetGetProcessEnv () throws Exception
  {
    Graph graph = ensureLoaded( "two-node" );
    GraphProcess p = engine.startProcess( graph );

    String expected = "bar";
    p.getEnv().setTransientAttribute( "foo", expected );
    String actual = (String)p.getEnv().getTransientAttribute( "foo" );

    Assert.assertEquals( expected, actual );
  }

  @Test
  public void testSetGetTokenEnvBetweenTokens () throws Exception
  {
    Graph graph = ensureLoaded( "two-node" );
    GraphProcess p = engine.startProcess( graph );

    NodeToken t = getActiveToken( p, "nodeA");

    String expected = "bar";
    t.getEnv().setTransientAttribute( "foo", expected );

    engine.complete( t, Arc.DEFAULT_ARC );

    t = getActiveToken( p, "nodeB");
    String actual = (String)t.getEnv().getTransientAttribute( "foo" );

    Assert.assertEquals( expected, actual );
  }

  @Test
  public void testSetGetTokenFullEnvBetweenTokens () throws Exception
  {
    Graph graph = ensureLoaded( "two-node" );
    GraphProcess p = engine.startProcess( graph );

    NodeToken t = getActiveToken( p, "nodeA");

    String expected = "bar";
    t.getFullEnv().setTransientAttribute( "foo", expected );

    engine.complete( t, Arc.DEFAULT_ARC );

    t = getActiveToken( p, "nodeB");
    String actual = (String)t.getFullEnv().getTransientAttribute( "foo" );

    Assert.assertEquals( expected, actual );
  }

  @Test
  public void testSetGetProcessEnvBetweenTokens () throws Exception
  {
    Graph graph = ensureLoaded( "two-node" );
    GraphProcess p = engine.startProcess( graph );

    NodeToken t = getActiveToken( p, "nodeA");

    String expected = "bar";
    p.getEnv().setTransientAttribute( "foo", expected );

    engine.complete( t, Arc.DEFAULT_ARC );

    t = getActiveToken( p, "nodeB");
    String actual = (String)p.getEnv().getTransientAttribute( "foo" );

    Assert.assertEquals( expected, actual );
  }
}
