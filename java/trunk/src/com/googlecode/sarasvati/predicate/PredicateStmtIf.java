package com.googlecode.sarasvati.predicate;

import com.googlecode.sarasvati.predicate.visitor.PredicateVisitor;

public class PredicateStmtIf implements PredicateStmt
{
  protected PredicateExpr expr;
  protected PredicateStmt ifStmt;
  protected PredicateStmt elseStmt;

  public PredicateStmtIf (PredicateExpr expr, PredicateStmt ifStmt, PredicateStmt elseStmt)
  {
    this.expr = expr;
    this.ifStmt = ifStmt;
    this.elseStmt = elseStmt;
  }

  public PredicateExpr getExpr ()
  {
    return expr;
  }

  public PredicateStmt getIfStmt ()
  {
    return ifStmt;
  }

  public PredicateStmt getElseStmt ()
  {
    return elseStmt;
  }

  @Override
  public Object eval (PredicateEnv env)
  {
    return expr.eval( env ) ? ifStmt.eval( env ) : elseStmt.eval( env );
  }

  @Override
  public void traverse (PredicateVisitor visitor)
  {
    visitor.visit( this );
    expr.traverse( visitor );
    ifStmt.traverse( visitor );
    elseStmt.traverse( visitor );
  }
}