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

    Copyright 2009 Paul Lorenz
*/
package com.googlecode.sarasvati.join.lang;

import com.googlecode.sarasvati.rubric.lang.RubricExpr;


public class IfJoinExpr extends AbstractJoinLangExpr
{
  protected RubricExpr expr;
  protected JoinLangExpr ifExpr;
  protected JoinLangExpr elseExpr;

  public IfJoinExpr (RubricExpr expr, JoinLangExpr ifExpr, JoinLangExpr elseExpr)
  {
    this.expr = expr;
    this.ifExpr = ifExpr;
    this.elseExpr = elseExpr;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinRequirement#isRequirementMet(com.googlecode.sarasvati.join.lang.JoinLangEnv)
   */
  @Override
  public boolean isSatisfied (JoinLangEnv joinEnv)
  {
    JoinLangExpr joinExpr = expr.eval( joinEnv ) ? ifExpr : elseExpr;
    return joinExpr.isSatisfied(  joinEnv );
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.AbstractJoinLangExpr#asOf()
   */
  @Override
  public IfJoinExpr asIf ()
  {
    return this;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.AbstractJoinLangExpr#isIf()
   */
  @Override
  public boolean isIf ()
  {
    return true;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangExpr#isEqualTo(com.googlecode.sarasvati.join.lang.JoinLangExpr)
   */
  @Override
  public boolean isEqualTo (final JoinLangExpr joinExpr)
  {
    if ( !joinExpr.isIf() )
    {
      return false;
    }

    IfJoinExpr other = joinExpr.asIf();
    return expr.isEqualTo( other.expr ) && ifExpr.isEqualTo( other.ifExpr ) && elseExpr.isEqualTo( other.elseExpr );
  }

  @Override
  public String toString()
  {
    return "if " + expr + " then " + ifExpr + " else " + elseExpr;
  }
}
