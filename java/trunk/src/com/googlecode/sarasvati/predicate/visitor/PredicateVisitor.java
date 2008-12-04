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

public interface PredicateVisitor
{
  void visit (PredicateStmtIf ifStmt);

  void visit (PredicateStmtDateSymbol dateSymbolStmt);

  void visit (PredicateStmtRelativeDate relativeDateStmt);

  void visit (PredicateStmtResult resultStmt);

  void visit (PredicateExprAnd andExpr);

  void visit (PredicateExprNot notExpr);

  void visit (PredicateExprOr orExpr);

  void visit (PredicateExprSymbol symbolExpr);
}
