package com.googlecode.sarasvati.predicate;

import com.googlecode.sarasvati.predicate.visitor.PredicateVisitor;

public class PredicateExprNot implements PredicateExpr
{
  protected PredicateExpr expr;

  public PredicateExprNot (PredicateExpr expr)
  {
    this.expr = expr;
  }

  public PredicateExpr getExpr ()
  {
    return expr;
  }

  @Override
  public boolean eval (PredicateEnv env)
  {
    return !expr.eval( env );
  }

  @Override
  public void traverse (PredicateVisitor visitor)
  {
    visitor.visit( this );
    expr.traverse( visitor );
  }
}
