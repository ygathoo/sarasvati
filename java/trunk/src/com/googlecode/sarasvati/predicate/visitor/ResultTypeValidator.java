/**
 * Created on Dec 4, 2008
 */
package com.googlecode.sarasvati.predicate.visitor;

import java.util.Date;

import com.googlecode.sarasvati.predicate.PredicateStmt;
import com.googlecode.sarasvati.predicate.PredicateStmtDateSymbol;
import com.googlecode.sarasvati.predicate.PredicateStmtRelativeDate;
import com.googlecode.sarasvati.predicate.PredicateStmtResult;

public class ResultTypeValidator extends PredicateVisitorAdaptor
{
  public static boolean isResultOfType (PredicateStmt stmt, Class<?> type)
  {
    ResultTypeValidator validator = new ResultTypeValidator( type );
    stmt.traverse( validator );
    return validator.isAllResultsMatchType();
  }

  protected Class<?> type;
  protected boolean allMatch = false;

  public ResultTypeValidator (Class<?> type)
  {
    this.type = type;
  }

  public boolean isAllResultsMatchType ()
  {
    return allMatch;
  }

  @Override
  public void visit (PredicateStmtResult resultStmt)
  {
    if ( !type.isAssignableFrom( resultStmt.getResult().getClass() ) )
    {
      allMatch = false;
    }
  }

  @Override
  public void visit (PredicateStmtDateSymbol dateSymbolStmt)
  {
    if ( !type.isAssignableFrom( Date.class ) )
    {
      allMatch = false;
    }
  }

  @Override
  public void visit (PredicateStmtRelativeDate relativeDateStmt)
  {
    if ( !type.isAssignableFrom( Date.class ) )
    {
      allMatch = false;
    }
  }
}
