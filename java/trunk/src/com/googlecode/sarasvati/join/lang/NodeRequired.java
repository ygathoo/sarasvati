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

import com.googlecode.sarasvati.ArcToken;

public class NodeRequired extends AbstractJoinRequirement
{
  private String nodeName;

  public NodeRequired (final String nodeName)
  {
    this.nodeName = nodeName;
  }

  @Override
  public boolean isSatisfied (final JoinLangEnv env)
  {
    for ( ArcToken token : env.getAvailableTokens() )
    {
      if ( nodeName.equals( token.getArc().getStartNode().getName() ) )
      {
        return true;
      }
    }

    return false;
  }

  /**
   * @see com.googlecode.sarasvati.join.lang.AbstractJoinRequirement#isEqualTo(com.googlecode.sarasvati.join.lang.JoinLangExpr)
   */
  @Override
  public boolean isEqualTo (final JoinLangExpr expr)
  {
    if ( !super.isEqualTo( expr ) )
    {
      return false;
    }

    if ( !( expr instanceof NodeRequired) )
    {
      return false;
    }

    NodeRequired other = (NodeRequired)expr;
    return nodeName.equals( other.nodeName );
  }

  @Override
  public String toString ()
  {
    return "require node " + nodeName + (getWhenExpr() == null ? "" : " when (" + getWhenExpr() +")" );
  }
}