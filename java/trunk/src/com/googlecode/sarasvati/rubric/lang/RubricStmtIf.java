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

package com.googlecode.sarasvati.rubric.lang;

import com.googlecode.sarasvati.rubric.env.RubricEnv;
import com.googlecode.sarasvati.rubric.visitor.RubricVisitor;

public class RubricStmtIf implements RubricStmt
{
  protected RubricExpr expr;
  protected RubricStmt ifStmt;
  protected RubricStmt elseStmt;

  public RubricStmtIf (RubricExpr expr, RubricStmt ifStmt, RubricStmt elseStmt)
  {
    this.expr = expr;
    this.ifStmt = ifStmt;
    this.elseStmt = elseStmt;
  }

  public RubricExpr getExpr ()
  {
    return expr;
  }

  public void setExpr (RubricExpr expr)
  {
    this.expr = expr;
  }

  public RubricStmt getIfStmt ()
  {
    return ifStmt;
  }

  public void setIfStmt (RubricStmt ifStmt)
  {
    this.ifStmt = ifStmt;
  }

  public RubricStmt getElseStmt ()
  {
    return elseStmt;
  }

  public void setElseStmt (RubricStmt elseStmt)
  {
    this.elseStmt = elseStmt;
  }

  @Override
  public Object eval (RubricEnv env)
  {
    return expr.eval( env ) ? ifStmt.eval( env ) : elseStmt.eval( env );
  }

  @Override
  public void traverse (RubricVisitor visitor)
  {
    visitor.visit( this );
    expr.traverse( visitor );
    ifStmt.traverse( visitor );
    elseStmt.traverse( visitor );
  }
}