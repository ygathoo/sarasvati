package com.googlecode.sarasvati.predicate;

public class PredicateExprSymbol implements PredicateExpr
{
  protected String symbol;

  public PredicateExprSymbol (String symbol)
  {
    this.symbol = symbol;
  }

  @Override
  public boolean eval (PredicateEnv env)
  {
    return env.evalPredicate( symbol );
  }
}
