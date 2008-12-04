package com.googlecode.sarasvati.predicate;

import com.googlecode.sarasvati.predicate.visitor.PredicateVisitor;

public class PredicateExprSymbol implements PredicateExpr
{
  protected String symbol;

  public PredicateExprSymbol (String symbol)
  {
    this.symbol = symbol;
  }

  public String getSymbol ()
  {
    return symbol;
  }

  @Override
  public boolean eval (PredicateEnv env)
  {
    return env.evalPredicate( symbol );
  }

  @Override
  public void traverse (PredicateVisitor visitor)
  {
    visitor.visit( this );
  }
}
