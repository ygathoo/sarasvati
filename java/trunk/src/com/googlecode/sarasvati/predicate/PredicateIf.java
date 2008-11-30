package com.googlecode.sarasvati.predicate;

public class PredicateIf implements PredicateStmt
{
  protected PredicateExpr expr;
  protected PredicateStmt ifStmt;
  protected PredicateStmt elseStmt;

  public PredicateIf (PredicateExpr expr, PredicateStmt ifStmt, PredicateStmt elseStmt)
  {
    this.expr = expr;
    this.ifStmt = ifStmt;
    this.elseStmt = elseStmt;
  }

  @Override
  public StmtResult eval (PredicateEnv env)
  {
    return expr.eval( env ) ? ifStmt.eval( env ) : elseStmt.eval( env );
  }
}
