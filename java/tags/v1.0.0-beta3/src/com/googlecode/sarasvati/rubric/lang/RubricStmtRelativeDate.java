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

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.rubric.env.RubricEnv;
import com.googlecode.sarasvati.rubric.visitor.RubricVisitor;

public class RubricStmtRelativeDate implements RubricStmt
{
  private static Map<String, Integer> unitMapping = new HashMap<String, Integer>();

  static
  {
    unitMapping.put( "hour", Calendar.HOUR );
    unitMapping.put( "hours", Calendar.HOUR );
    unitMapping.put( "day", Calendar.DATE );
    unitMapping.put( "days", Calendar.DATE );
    unitMapping.put( "week", Calendar.WEEK_OF_YEAR );
    unitMapping.put( "weeks", Calendar.WEEK_OF_YEAR );
  }

  protected int offset;
  protected int unit;
  protected RubricStmtDateSymbol dateSymbolExpr;

  public RubricStmtRelativeDate (int offset, String unitName, String type, String symbol)
  {
    this.offset = offset;

    if ( "before".equals( type ) )
    {
      this.offset = -offset;
    }

    this.unit = unitMapping.get( unitName );
    this.dateSymbolExpr = new RubricStmtDateSymbol( symbol );
  }

  public int getOffset ()
  {
    return offset;
  }

  public int getUnit ()
  {
    return unit;
  }

  public RubricStmtDateSymbol getDateSymbolExpr ()
  {
    return dateSymbolExpr;
  }

  @Override
  public Date eval (RubricEnv env)
  {
    Date baseDate = dateSymbolExpr.eval( env );
    return env.evalRelativeDate( baseDate, offset, unit );
  }

  @Override
  public void traverse (RubricVisitor visitor)
  {
    visitor.visit( this );
    dateSymbolExpr.traverse( visitor );
  }
}