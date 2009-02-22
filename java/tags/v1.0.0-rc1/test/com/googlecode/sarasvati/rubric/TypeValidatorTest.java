/**
 * Created on Dec 18, 2008
 */
package com.googlecode.sarasvati.rubric;

import java.util.Date;

import junit.framework.Assert;

import org.junit.Test;

import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.rubric.lang.RubricStmt;
import com.googlecode.sarasvati.rubric.visitor.ResultTypeValidator;

public class TypeValidatorTest
{
  @Test public void testStringCheckOnString ()
  {
    String program = "\"foo\"";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertTrue( "All result type should be string", validator.isAllResultsMatchType() );

    program = "if a or b then \"foo\" else \"bar\"";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertTrue( "All result type should be string", validator.isAllResultsMatchType() );
  }

  @Test public void testStringCheckOnNumber ()
  {
    String program = "1";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect int", validator.isAllResultsMatchType() );

    program = "if a or b then \"foo\" else 2";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect int", validator.isAllResultsMatchType() );
  }

  @Test public void testStringCheckOnDate ()
  {
    String program = "(now)";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect date", validator.isAllResultsMatchType() );

    program = "if a or b then \"foo\" else (1 day before now)";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect date", validator.isAllResultsMatchType() );
  }

  @Test public void testStringCheckOnGuardResponse ()
  {
    String program = "Accept";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect guard response", validator.isAllResultsMatchType() );

    program = "if a or b then \"foo\" else Discard";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( String.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect guard response", validator.isAllResultsMatchType() );
  }

  @Test public void testGuardResponseCheckOnGuardResponse ()
  {
    String program = "Accept";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertTrue( "All result type should be guard response", validator.isAllResultsMatchType() );

    program = "if a or b then Discard else Accept";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertTrue( "All result type should be guard response", validator.isAllResultsMatchType() );
  }

  @Test public void testGuardResponseCheckOnNumber ()
  {
    String program = "1";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect int", validator.isAllResultsMatchType() );

    program = "if a or b then Accept else 2";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect int", validator.isAllResultsMatchType() );
  }

  @Test public void testGuardResponseCheckOnDate ()
  {
    String program = "(now)";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect date", validator.isAllResultsMatchType() );

    program = "if a or b then Accept else (1 day before now)";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect date", validator.isAllResultsMatchType() );
  }

  @Test public void testGuardResponseCheckOnString ()
  {
    String program = "\"foo\"";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect guard response", validator.isAllResultsMatchType() );

    program = "if a or b then \"foo\" else Discard";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( GuardResponse.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect guard response", validator.isAllResultsMatchType() );
  }

  @Test public void testDateCheckOnDate ()
  {
    String program = "(now)";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertTrue( "All result type should be date", validator.isAllResultsMatchType() );

    program = "if a or b then (now) else (1 day after now)";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertTrue( "All result type should be date", validator.isAllResultsMatchType() );
  }

  @Test public void testDateCheckOnNumber ()
  {
    String program = "1";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect int", validator.isAllResultsMatchType() );

    program = "if a or b then (now) else 2";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect int", validator.isAllResultsMatchType() );
  }

  @Test public void testDateCheckOnGuardResponse ()
  {
    String program = "Accept";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect date", validator.isAllResultsMatchType() );

    program = "if a or b then Accept else (1 day before now)";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect date", validator.isAllResultsMatchType() );
  }

  @Test public void testDateCheckOnString ()
  {
    String program = "\"foo\"";
    RubricStmt stmt = RubricInterpreter.compile( program );
    ResultTypeValidator validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect guard response", validator.isAllResultsMatchType() );

    program = "if a or b then \"foo\" else (now)";
    stmt = RubricInterpreter.compile( program );
    validator = new ResultTypeValidator( Date.class );
    stmt.traverse( validator );
    Assert.assertFalse( "Should detect guard response", validator.isAllResultsMatchType() );
  }
}
