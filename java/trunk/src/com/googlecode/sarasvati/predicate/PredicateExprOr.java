package com.googlecode.sarasvati.predicate;

import com.googlecode.sarasvati.predicate.visitor.PredicateVisitor;

public class PredicateExprOr implements PredicateExpr
{
  protected PredicateExpr left;
  protected PredicateExpr right;

  public PredicateExprOr (PredicateExpr left, PredicateExpr right)
  {
    this.left = left;
    this.right = right;
  }

  public PredicateExpr getLeft ()
  {
    return left;
  }

  public PredicateExpr getRight ()
  {
    return right;
  }

  @Override
  public boolean eval (PredicateEnv env)
  {
    return left.eval( env ) || right.eval( env );
  }

  @Override
  public void traverse (PredicateVisitor visitor)
  {
    visitor.visit( this );
    left.traverse( visitor );
    right.traverse( visitor );
  }
}
