package com.googlecode.sarasvati.predicate;

public interface PredicateStmt
{
  StmtResult eval (PredicateEnv env);
}
