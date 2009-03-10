package com.googlecode.sarasvati.rubric;

import junit.framework.Assert;

import org.junit.Test;

public class RubricStringTest
{
  @Test public void testSimple ()
  {
    String expected = "foo";
    String script="\"foo\"";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }

  @Test public void testWithQuotes ()
  {
    String expected = "foo\"bar\"";
    String script="\"foo\\\"bar\\\"\"";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }

  @Test (expected=RubricException.class)
  public void testUnbalancedQuoteFailure ()
  {
    String script="\"foo";
    System.out.println( "SCRIPT: " + script );
    RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );
  }

  @Test public void testWithSpaces ()
  {
    String expected = "This is a long statement, include a \"quote with stuff\"";
    String script="\"This is a long statement, include a \\\"quote with stuff\\\"\"";
    System.out.println( "SCRIPT: " + script );

    Object result = RubricInterpreter.compile( script ).eval( TestRubricEnv.INSTANCE );

    Assert.assertEquals( expected, result );
  }
}
