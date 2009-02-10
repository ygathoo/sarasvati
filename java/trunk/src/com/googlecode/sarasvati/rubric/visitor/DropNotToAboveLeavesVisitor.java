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
package com.googlecode.sarasvati.rubric.visitor;

import com.googlecode.sarasvati.rubric.lang.RubricExpr;
import com.googlecode.sarasvati.rubric.lang.RubricExprAnd;
import com.googlecode.sarasvati.rubric.lang.RubricExprNot;
import com.googlecode.sarasvati.rubric.lang.RubricExprOr;
import com.googlecode.sarasvati.rubric.lang.RubricStmtIf;

public class DropNotToAboveLeavesVisitor extends RubricVisitorAdaptor
{
  @Override
  public void visit (RubricStmtIf ifStmt)
  {
    if ( ifStmt.getExpr().isNot() )
    {
      ifStmt.setExpr( pushDownNot( ifStmt.getExpr().toNot() ) );
    }
  }

  @Override
  public void visit (RubricExprAnd andExpr)
  {
    if ( andExpr.getLeft().isNot() )
    {
      andExpr.setLeft( pushDownNot( andExpr.getLeft().toNot() ) );
    }
    if ( andExpr.getRight().isNot() )
    {
      andExpr.setRight( pushDownNot( andExpr.getRight().toNot() ) );
    }
  }

  @Override
  public void visit (RubricExprOr orExpr)
  {
    if ( orExpr.getLeft().isNot() )
    {
      orExpr.setLeft( pushDownNot( orExpr.getLeft().toNot() ) );
    }
    if ( orExpr.getRight().isNot() )
    {
      orExpr.setRight( pushDownNot( orExpr.getRight().toNot() ) );
    }
  }

  protected RubricExpr pushDownNot (RubricExprNot notExpr)
  {
    RubricExpr expr = notExpr.getExpr();
    if ( expr.isAnd() )
    {
      RubricExprAnd andExpr = expr.toAnd();
      expr = new RubricExprOr( negate( andExpr.getLeft() ), negate( andExpr.getRight() ) );
    }
    if ( expr.isOr() )
    {
      RubricExprAnd andExpr = expr.toAnd();
      expr = new RubricExprAnd( negate( andExpr.getLeft() ), negate( andExpr.getRight() ) );
    }
    return expr;
  }

  protected RubricExpr negate (RubricExpr expr)
  {
    return expr.isNot() ? expr.toNot().getExpr() : new RubricExprNot( expr );
  }
}