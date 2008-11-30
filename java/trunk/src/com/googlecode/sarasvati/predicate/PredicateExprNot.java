package com.googlecode.sarasvati.predicate;

public class PredicateExprNot implements PredicateExpr
{
  protected PredicateExpr expr;

  public PredicateExprNot (PredicateExpr expr)
  {
    this.expr = expr;
  }

  @Override
  public boolean eval (PredicateEnv env)
  {
    return !expr.eval( env );
  }
}
