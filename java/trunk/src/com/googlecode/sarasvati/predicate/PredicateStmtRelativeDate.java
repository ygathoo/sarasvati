package com.googlecode.sarasvati.predicate;

import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

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
  protected String symbol;

  public PredicateStmtRelativeDate (int offset, String unitName, String type, String symbol)
  {
    this.offset = offset;

    if ( "before".equals( type ) )
    {
      offset = -offset;
    }

    this.unit = unitMapping.get( unitName );
    this.symbol = symbol;
  }

  @Override
  public Date eval (PredicateEnv env)
  {
    Date baseDate = env.evalDate( symbol );
    return env.evalRelative( baseDate, offset, unit );
  }
}