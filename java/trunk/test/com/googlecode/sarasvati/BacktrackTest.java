package com.googlecode.sarasvati;

import java.io.File;
import java.util.Collection;

import junit.framework.Assert;

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

    Assert.assertTrue( "Should contain one node token", tokens.size() == 1 );

    NodeToken tokenA = tokens.iterator().next();

    Assert.assertEquals( "A", tokenA.getNode().getName() );

    engine.completeExecution( tokenA, Arc.DEFAULT_ARC );

    tokens = p.getActiveNodeTokens();

    Assert.assertTrue( "Should contain one node token", tokens.size() == 1 );

    NodeToken tokenB = tokens.iterator().next();

    Assert.assertEquals( "B", tokenB.getNode().getName() );

    engine.backtrack( tokenA );

    tokens = p.getActiveNodeTokens();

    Assert.assertTrue( "Token B should be backtracked", tokenB.getExecutionType().isBacktracked() );
    Assert.assertTrue( "Token A should be backtracked", tokenB.getExecutionType().isBacktracked() );

    tokens = p.getActiveNodeTokens();
    Assert.assertTrue( "Should contain one node token", tokens.size() == 1 );

    NodeToken tokenANew = tokens.iterator().next();

    Assert.assertEquals( "A", tokenANew.getNode().getName() );
    Assert.assertFalse( "New Token A should be incomplete", tokenANew.isComplete() );
    Assert.assertEquals( 1, tokenANew.getParentTokens().size() );

    ArcToken arcParent = tokenANew.getParentTokens().get( 0 );

    Assert.assertEquals( ExecutionType.BackwardBacktracked, arcParent.getExecutionType() );
    Assert.assertEquals( tokenB, arcParent.getParentToken() );
    Assert.assertEquals( 1, tokenB.getParentTokens().size() );

    arcParent = tokenB.getParentTokens().get( 0 );

    Assert.assertEquals( ExecutionType.ForwardBacktracked, arcParent.getExecutionType() );
    Assert.assertEquals( tokenA, arcParent.getParentToken() );
  }
}