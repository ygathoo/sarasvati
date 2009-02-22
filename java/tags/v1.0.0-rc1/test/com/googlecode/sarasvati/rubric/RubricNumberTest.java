package com.googlecode.sarasvati.rubric;

import junit.framework.Assert;

import org.junit.Test;

public class RubricNumberTest
{
  @Test public void testSimple ()
  {
    Integer expected = 1;
    String script="1";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }

  @Test public void testNegative ()
  {
    Integer expected = -10000;
    String script="-10000";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }
}
