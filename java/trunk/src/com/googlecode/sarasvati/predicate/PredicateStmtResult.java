package com.googlecode.sarasvati.predicate;

public class PredicateStmtResult implements PredicateStmt
{
  protected Object result;

  public PredicateStmtResult (Object result)
  {
    this.result = result;
  }

  @Override
  public Object eval (PredicateEnv env)
  {
    return result;
  }
}
