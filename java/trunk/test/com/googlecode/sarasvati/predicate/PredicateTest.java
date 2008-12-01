package com.googlecode.sarasvati.predicate;

import java.util.Calendar;
import java.util.Date;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;

public class PredicateTest
{
  public static void eval (String testStmt) throws Exception
  {
    PredicateLexer lexer = new PredicateLexer( new ANTLRStringStream( testStmt ) );

    CommonTokenStream stream = new CommonTokenStream( lexer );
    PredicateParser parser = new PredicateParser( stream );

    PredicateStmt stmt = parser.program().value;

    PredicateEnv env = new PredicateEnv()
    {
      @Override
      public Date evalRelative (Date date, int offset, int unit)
      {
        Calendar cal = Calendar.getInstance();
        cal.setTime( date );
        cal.add( unit, offset );
        return cal.getTime();
      }

      @Override
      public boolean evalPredicate (String predicateName)
      {
        if ( "a".equals( predicateName ) )
        {
          return true;
        }

        if ( "b".equals( predicateName ) )
        {
          return false;
        }

        throw new IllegalArgumentException( "No such named predicate: " + predicateName );
      }

      @Override
      public Date evalDate (String dateName)
      {
        if ( "now".equals( dateName ) )
        {
          return new Date();
        }

        throw new IllegalArgumentException( "No such named date: " + dateName );
      }
    };

    System.out.println( stmt.eval( env ) );
  }

  public static void main(String[] args) throws Exception
  {
    String[] tests = { "if a or b then (1 week after now) else Discard",
                       "\"Hello \\\"there!\"",
                       "foo",
                       "50",
                       "(5 hours before now)" };

    for ( String test : tests )
    {
      eval( test );
    }
  }
}
