package com.googlecode.sarasvati.predicate;

import com.googlecode.sarasvati.predicate.visitor.PredicateVisitor;

public class PredicateStmtResult implements PredicateStmt
{
  protected Object result;

  public PredicateStmtResult (Object result)
  {
    this.result = result;
  }

  public Object getResult ()
  {
    return result;
  }

  @Override
  public Object eval (PredicateEnv env)
  {
    return result;
  }

  @Override
  public void traverse (PredicateVisitor visitor)
  {
    visitor.visit( this );
  }
}
