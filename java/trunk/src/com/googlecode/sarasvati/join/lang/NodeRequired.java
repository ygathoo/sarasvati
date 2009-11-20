/**
 * Created on Nov 20, 2009
 */
package com.googlecode.sarasvati.join.lang;

import com.googlecode.sarasvati.rubric.lang.RubricExpr;

public class NodeRequired implements JoinLangRequirement
{
  private String nodeName;
  private RubricExpr whenExpr;

  public NodeRequired (final String nodeName)
  {
    this.nodeName = nodeName;
  }

  @Override
  public boolean isRequirementMet ()
  {
    return false;
  }

  public RubricExpr getWhenExpr ()
  {
    return whenExpr;
  }

  public void setWhenExpr (final RubricExpr whenExpr)
  {
    this.whenExpr = whenExpr;
  }
}
