/**
 * Created on Dec 4, 2008
 */
package com.googlecode.sarasvati.predicate.visitor;

import com.googlecode.sarasvati.predicate.PredicateExprAnd;
import com.googlecode.sarasvati.predicate.PredicateExprNot;
import com.googlecode.sarasvati.predicate.PredicateExprOr;
import com.googlecode.sarasvati.predicate.PredicateExprSymbol;
import com.googlecode.sarasvati.predicate.PredicateStmtDateSymbol;
import com.googlecode.sarasvati.predicate.PredicateStmtIf;
import com.googlecode.sarasvati.predicate.PredicateStmtRelativeDate;
import com.googlecode.sarasvati.predicate.PredicateStmtResult;

public class PredicateVisitorAdaptor implements PredicateVisitor
{
  @Override
  public void visit (PredicateStmtIf ifStmt)
  {
    // does nothing by default
  }

  @Override
  public void visit (PredicateStmtDateSymbol dateSymbolStmt)
  {
    // does nothing by default
  }

  @Override
  public void visit (PredicateStmtRelativeDate relativeDateStmt)
  {
    // does nothing by default
  }

  @Override
  public void visit (PredicateStmtResult resultStmt)
  {
    // does nothing by default
  }

  @Override
  public void visit (PredicateExprAnd andExpr)
  {
    // does nothing by default
  }

  @Override
  public void visit (PredicateExprNot notExpr)
  {
    // does nothing by default
  }

  @Override
  public void visit (PredicateExprOr orExpr)
  {
    // does nothing by default
  }

  @Override
  public void visit (PredicateExprSymbol symbolExpr)
  {
    // does nothing by default
  }
}