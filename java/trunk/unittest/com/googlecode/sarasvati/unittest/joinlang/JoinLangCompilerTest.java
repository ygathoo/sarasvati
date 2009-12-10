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

package com.googlecode.sarasvati.unittest.joinlang;

import junit.framework.Assert;

import org.junit.Test;

import com.googlecode.sarasvati.join.JoinLangInterpreter;
import com.googlecode.sarasvati.join.lang.AndJoinExpr;
import com.googlecode.sarasvati.join.lang.JoinLangExpr;
import com.googlecode.sarasvati.join.lang.NodeRequired;
import com.googlecode.sarasvati.join.lang.OrJoinExpr;
import com.googlecode.sarasvati.rubric.lang.RubricExprAnd;
import com.googlecode.sarasvati.rubric.lang.RubricExprNot;
import com.googlecode.sarasvati.rubric.lang.RubricExprOr;
import com.googlecode.sarasvati.rubric.lang.RubricExprSymbol;

public class JoinLangCompilerTest
{
  @Test public void testRequireNode ()
  {
    AndJoinExpr expr = new AndJoinExpr( new NodeRequired( "foo" ) );

    String script = "require node \"foo\"";
    System.out.println( "SCRIPT: " + script );

    JoinLangExpr result = JoinLangInterpreter.compile( script );
    Assert.assertTrue( expr.isEqualTo( result ) );
  }

  @Test public void testRequireNodeWithWhen ()
  {
    NodeRequired nodeRequired = new NodeRequired( "foo" );
    nodeRequired.setWhenExpr( new RubricExprSymbol( "Order.isExpedited" ) );
    AndJoinExpr expr = new AndJoinExpr( nodeRequired );

    String script = "require node \"foo\" when Order.isExpedited";
    System.out.println( "SCRIPT: " + script );

    JoinLangExpr result = JoinLangInterpreter.compile( script );
    Assert.assertTrue( "Result does not match expected", expr.isEqualTo( result ) );
  }

  @Test public void testMultiRequireNode ()
  {
    AndJoinExpr expr = new AndJoinExpr( new NodeRequired( "foo" ) );
    expr.add( new NodeRequired( "bar" ) );

    String script = "require node \"foo\"\n" +
                    "require node \"bar\"";

    System.out.println( "SCRIPT: " + script );

    JoinLangExpr result = JoinLangInterpreter.compile( script );
    Assert.assertTrue( "Result does not match expected", expr.isEqualTo( result ) );
  }

  @Test public void testMultiRequireNodeWithWhen ()
  {
    NodeRequired require1 = new NodeRequired( "foo" );
    require1.setWhenExpr( new RubricExprAnd( new RubricExprSymbol( "Order.isExpedited" ),
                                             new RubricExprSymbol( "Order.isNew" ) ) );
    AndJoinExpr expr = new AndJoinExpr( require1 );

    NodeRequired require2 = new NodeRequired( "bar" );
    require2.setWhenExpr( new RubricExprOr( new RubricExprNot( new RubricExprSymbol( "Order.isNew" ) ),
                                            new RubricExprSymbol( "Order.isExpedited" ) ) );
    expr.add( require2 );

    String script = "require node \"foo\" when Order.isExpedited and Order.isNew \n" +
                    "require node \"bar\" when not Order.isNew or Order.isExpedited";

    System.out.println( "SCRIPT:\n " + script );

    JoinLangExpr result = JoinLangInterpreter.compile( script );
    System.out.println( "Expected:\n"+ expr );
    System.out.println( "Result:\n" + result );
    Assert.assertTrue( "Result does not match expected", expr.isEqualTo( result ) );
  }

  @Test public void testAmbiguousOr ()
  {
    NodeRequired require1 = new NodeRequired( "foo" );
    require1.setWhenExpr( new RubricExprOr( new RubricExprSymbol( "Order.isExpedited" ),
                                             new RubricExprSymbol( "Order.isNew" ) ) );
    AndJoinExpr expr1 = new AndJoinExpr( require1 );

    NodeRequired require2 = new NodeRequired( "bar" );
    require2.setWhenExpr( new RubricExprOr( new RubricExprNot( new RubricExprSymbol( "Order.isNew" ) ),
                                            new RubricExprSymbol( "Order.isExpedited" ) ) );
    AndJoinExpr expr2 = new AndJoinExpr( require2 );
    OrJoinExpr orExpr = new OrJoinExpr( expr1, expr2 );

    String script = "require node \"foo\" when Order.isExpedited or Order.isNew \n" +
                    "or\n" +
                    "require node \"bar\" when not Order.isNew or Order.isExpedited";

    System.out.println( "SCRIPT:\n " + script );

    JoinLangExpr result = JoinLangInterpreter.compile( script );
    System.out.println( "Expected:\n"+ orExpr );
    System.out.println( "Result:\n" + result );
    Assert.assertTrue( "Result does not match expected", orExpr.isEqualTo( result ) );
  }
}