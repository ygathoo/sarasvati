/**
 * Created on Feb 11, 2009
 */
package com.googlecode.sarasvati.rubric;

import org.junit.Test;

import junit.framework.Assert;

import com.googlecode.sarasvati.rubric.lang.RubricStmt;
import com.googlecode.sarasvati.rubric.visitor.DropNotToAboveLeavesVisitor;

public class NotPushDownTest
{
  @Test public void testOrPushdown ()
  {
    String a = "if not (isThis or isThat) then Accept else Discard";
    String b = "if not isThis and not isThat then Accept else Discard";

    RubricStmt stmtA = RubricInterpreter.compile( a );
    RubricStmt stmtB = RubricInterpreter.compile( b );

    DropNotToAboveLeavesVisitor.process( stmtA );
    Assert.assertTrue( "stmts should match", stmtA.isEqualTo( stmtB ) );
  }

  @Test public void testAndPushdown ()
  {
    String a = "if not (isThis and isThat) then Accept else Discard";
    String b = "if not isThis or not isThat then Accept else Discard";

    RubricStmt stmtA = RubricInterpreter.compile( a );
    RubricStmt stmtB = RubricInterpreter.compile( b );

    DropNotToAboveLeavesVisitor.process( stmtA );
    Assert.assertTrue( "stmts should match", stmtA.isEqualTo( stmtB ) );
  }

  @Test public void testNestedOrPushdown ()
  {
    String a = "if foo and not (isThis or isThat) then Accept else Discard";
    String b = "if foo and (not isThis and not isThat) then Accept else Discard";

    RubricStmt stmtA = RubricInterpreter.compile( a );
    RubricStmt stmtB = RubricInterpreter.compile( b );

    DropNotToAboveLeavesVisitor.process( stmtA );
    Assert.assertTrue( "stmts should match", stmtA.isEqualTo( stmtB ) );
  }

  @Test public void testNestedAndPushdown ()
  {
    String a = "if foo and not (isThis and isThat) then Accept else Discard";
    String b = "if foo and (not isThis or not isThat) then Accept else Discard";

    RubricStmt stmtA = RubricInterpreter.compile( a );
    RubricStmt stmtB = RubricInterpreter.compile( b );

    DropNotToAboveLeavesVisitor.process( stmtA );
    Assert.assertTrue( "stmts should match", stmtA.isEqualTo( stmtB ) );
  }

  @Test public void testOrPushdownWithNot ()
  {
    String a = "if not (isThis or not isThat) then Accept else Discard";
    String b = "if not isThis and isThat then Accept else Discard";

    RubricStmt stmtA = RubricInterpreter.compile( a );
    RubricStmt stmtB = RubricInterpreter.compile( b );

    DropNotToAboveLeavesVisitor.process( stmtA );
    Assert.assertTrue( "stmts should match", stmtA.isEqualTo( stmtB ) );
  }

  @Test public void testAndPushdownWithNot ()
  {
    String a = "if not (isThis and not isThat) then Accept else Discard";
    String b = "if not isThis or isThat then Accept else Discard";

    RubricStmt stmtA = RubricInterpreter.compile( a );
    RubricStmt stmtB = RubricInterpreter.compile( b );

    DropNotToAboveLeavesVisitor.process( stmtA );
    Assert.assertTrue( "stmts should match", stmtA.isEqualTo( stmtB ) );
  }

  @Test public void testComplex()
  {
    String a = "if not ((not (not foo or not bar)) and not (bar and not baz)) then Accept else Discard";
    String b = "if (not foo or not bar) or (bar and not baz) then Accept else Discard";

    RubricStmt stmtA = RubricInterpreter.compile( a );
    RubricStmt stmtB = RubricInterpreter.compile( b );

    DropNotToAboveLeavesVisitor.process( stmtA );
    Assert.assertTrue( "stmts should match", stmtA.isEqualTo( stmtB ) );
  }
}
