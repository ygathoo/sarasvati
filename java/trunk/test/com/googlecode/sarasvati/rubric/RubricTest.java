package com.googlecode.sarasvati.rubric;

import java.util.Calendar;
import java.util.Date;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;

import com.googlecode.sarasvati.rubric.env.RubricEnv;
import com.googlecode.sarasvati.rubric.lang.ErrorReportingRubricParser;
import com.googlecode.sarasvati.rubric.lang.RubricLexer;
import com.googlecode.sarasvati.rubric.lang.RubricStmt;

public class RubricTest
{
  public static void eval (String testStmt) throws Exception
  {
    RubricLexer lexer = new RubricLexer( new ANTLRStringStream( testStmt ) );

    CommonTokenStream stream = new CommonTokenStream( lexer );
    ErrorReportingRubricParser parser = new ErrorReportingRubricParser( stream );

    RubricStmt stmt = parser.program().value;

    if ( parser.getError() != null )
    {
      throw parser.getError();
    }

    RubricEnv env = new RubricEnv()
    {
      @Override
      public Date evalRelativeDate (Date date, int offset, int unit)
      {
        Calendar cal = Calendar.getInstance();
        cal.setTime( date );
        cal.add( unit, offset );
        return cal.getTime();
      }

      @Override
      public boolean evalPredicate (String predicate)
      {
        if ( "a".equals( predicate ) )
        {
          return true;
        }

        if ( "b".equals( predicate ) )
        {
          return false;
        }

        System.out.println( "Evaluating guard predicate: " + predicate );
        return true;
      }

      @Override
      public Date evalDateFunction (String dateFunction)
      {
        if ( "now".equals( dateFunction ) )
        {
          return new Date();
        }

        throw new IllegalArgumentException( "No such named date: " + dateFunction );
      }
    };

    System.out.println( stmt.eval( env ) );
  }

  public static void main(String[] args) throws Exception
  {
    String[] tests = { "if a or b then (1 week after now) else Discard",
                       "\"Hello \\\"there!\"",
                       "\"foo\"",
                       "50",
                       "(5 hours before now)",
                       "if test1 or test2 then if test3 and not test4 then Accept else Skip else Skip reject",
                       "if not test1 or not test2 then if test3 and not test4 then Accept else Skip else Skip reject",
                       "if ( test1 and test2 ) then if test3 then Accept else Skip else Skip reject",
                       "if not ( test1 and test2 ) then if test3 then Accept else Skip else Skip reject",
                       "if ( not test1 and not test2 ) or (test3 and test4) then Discard else Skip",
                       "if (a or b then Accept else Discard",
                       "if (a or b) and c then 0 else 1"};

    for ( String test : tests )
    {
      eval( test );
    }
  }
}
