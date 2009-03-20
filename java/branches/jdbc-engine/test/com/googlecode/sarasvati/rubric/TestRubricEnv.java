package com.googlecode.sarasvati.rubric;

import java.util.Date;

import com.googlecode.sarasvati.rubric.env.RubricEnv;

public class TestRubricEnv implements RubricEnv
{
  public static final TestRubricEnv INSTANCE = new TestRubricEnv();

  @Override
  public Date evalDateFunction (String dateFunction)
  {
    return null;
  }

  @Override
  public boolean evalPredicate (String predicate)
  {
    return false;
  }

  @Override
  public Date evalRelativeDate (Date date, boolean business, int offset, int unit)
  {
    return null;
  }
}
