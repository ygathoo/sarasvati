package com.googlecode.sarasvati.predicate;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.predicate.visitor.PredicateVisitor;

public class PredicateStmtRelativeDate implements PredicateStmt
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
  protected PredicateStmtDateSymbol dateSymbolExpr;

  public PredicateStmtRelativeDate (int offset, String unitName, String type, String symbol)
  {
    this.offset = offset;

    if ( "before".equals( type ) )
    {
      this.offset = -offset;
    }

    this.unit = unitMapping.get( unitName );
    this.dateSymbolExpr = new PredicateStmtDateSymbol( symbol );
  }

  public int getOffset ()
  {
    return offset;
  }

  public int getUnit ()
  {
    return unit;
  }

  public PredicateStmtDateSymbol getDateSymbolExpr ()
  {
    return dateSymbolExpr;
  }

  @Override
  public Date eval (PredicateEnv env)
  {
    Date baseDate = dateSymbolExpr.eval( env );
    return env.evalRelative( baseDate, offset, unit );
  }

  @Override
  public void traverse (PredicateVisitor visitor)
  {
    visitor.visit( this );
    dateSymbolExpr.traverse( visitor );
  }
}