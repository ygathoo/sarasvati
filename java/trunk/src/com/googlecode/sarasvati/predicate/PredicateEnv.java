package com.googlecode.sarasvati.predicate;

import java.util.Date;

public interface PredicateEnv
{
  boolean evalPredicate (String predicateName);

  Date evalDate (String dateName);

  Date evalRelative (Date date, int offset, int unit);
}
