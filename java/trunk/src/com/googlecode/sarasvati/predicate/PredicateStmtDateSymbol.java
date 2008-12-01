package com.googlecode.sarasvati.predicate;

import java.util.Date;

public class PredicateStmtDateSymbol implements PredicateStmt
{
  protected String symbol;

  public PredicateStmtDateSymbol (String symbol)
  {
    this.symbol = symbol;
  }

  @Override
  public Date eval (PredicateEnv env)
  {
    return env.evalDate( symbol );
  }
}
