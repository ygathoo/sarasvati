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

import com.googlecode.sarasvati.rubric.env.PredicateEnv;
import com.googlecode.sarasvati.rubric.lang.RubricExpr;

public abstract class AbstractJoinRequirement extends AbstractJoinLangExpr implements JoinRequirement
{
  private RubricExpr whenExpr;

  /**
   * @return the whenExpr
   */
  public RubricExpr getWhenExpr ()
  {
    return whenExpr;
  }

  /**
   * @param whenExpr the whenExpr to set
   */
  public void setWhenExpr (RubricExpr whenExpr)
  {
    this.whenExpr = whenExpr;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinRequirement#isApplicable(com.googlecode.sarasvati.rubric.env.PredicateEnv)
   */
  @Override
  public boolean isApplicable (PredicateEnv env)
  {
    return whenExpr == null || whenExpr.eval( env );
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.AbstractJoinLangExpr#asJoinRequirement()
   */
  @Override
  public JoinRequirement asJoinRequirement ()
  {
    return this;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.AbstractJoinLangExpr#isJoinRequirement()
   */
  @Override
  public boolean isJoinRequirement ()
  {
    return true;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.JoinLangExpr#isEqualTo(com.googlecode.sarasvati.join.lang.JoinLangExpr)
   */
  @Override
  public boolean isEqualTo (JoinLangExpr expr)
  {
    if ( !expr.isJoinRequirement() )
    {
      return false;
    }

    JoinRequirement other = expr.asJoinRequirement();

    if ( whenExpr == null )
    {
      return other.getWhenExpr() == null;
    }

    return whenExpr.isEqualTo( other.getWhenExpr() );
  }
}
