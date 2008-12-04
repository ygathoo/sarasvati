package com.googlecode.sarasvati.predicate;

import java.util.Date;

import com.googlecode.sarasvati.predicate.visitor.PredicateVisitor;

public class PredicateStmtDateSymbol implements PredicateStmt
{
  protected String symbol;

  public PredicateStmtDateSymbol (String symbol)
  {
    this.symbol = symbol;
  }

  public String getSymbol ()
  {
    return symbol;
  }

  @Override
  public Date eval (PredicateEnv env)
  {
    return env.evalDate( symbol );
  }

  @Override
  public void traverse (PredicateVisitor visitor)
  {
    visitor.visit( this );
  }
}
