package com.googlecode.sarasvati.predicate;

public class PredicateExprOr implements PredicateExpr
{
  protected PredicateExpr left;
  protected PredicateExpr right;

  public PredicateExprOr (PredicateExpr left, PredicateExpr right)
  {
    this.left = left;
    this.right = right;
  }

  @Override
  public boolean eval (PredicateEnv env)
  {
    return left.eval( env ) || right.eval( env );
  }
}
