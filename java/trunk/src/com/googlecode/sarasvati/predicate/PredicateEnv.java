package com.googlecode.sarasvati.predicate;

import java.util.Date;

public interface PredicateEnv
{
  boolean evalPredicate (String predicate);

  Date evalDate (String namedDate);

  Date evalNow ();

  Date eval (int units, int amt, Date date);
}
