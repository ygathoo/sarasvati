package com.googlecode.sarasvati.rubric;

import junit.framework.Assert;

import org.junit.Test;

import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.SkipNodeGuardResponse;

public class RubricGuardResponseTest
{
  @Test public void testAccept ()
  {
    GuardResponse expected = GuardResponse.ACCEPT_TOKEN_RESPONSE;
    String script="Accept";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }

  @Test public void testDiscard ()
  {
    GuardResponse expected = GuardResponse.DISCARD_TOKEN_RESPONSE;
    String script="Discard";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }

  @Test public void testSkip ()
  {
    GuardResponse expected = SkipNodeGuardResponse.DEFAULT_ARC_SKIP_NODE_RESPONSE;
    String script="Skip";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }

  @Test public void testSkipWithArcName ()
  {
    GuardResponse expected = new SkipNodeGuardResponse( "testArc" );
    String script="Skip testArc";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }
}