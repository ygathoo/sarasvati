package com.googlecode.sarasvati.predicate;

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

  @Override
  public Object eval (PredicateEnv env)
  {
    return expr.eval( env ) ? ifStmt.eval( env ) : elseStmt.eval( env );
  }
}
