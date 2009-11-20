/**
 * Created on Nov 20, 2009
 */
package com.googlecode.sarasvati.join.lang;

import com.googlecode.sarasvati.rubric.lang.RubricExpr;

public interface JoinLangRequirement
{
  boolean isRequirementMet ();

  void setWhenExpr (RubricExpr expr);
}
