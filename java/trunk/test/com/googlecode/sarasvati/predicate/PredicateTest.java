package com.googlecode.sarasvati.predicate;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;

public class PredicateTest
{
  public static void main(String[] args) throws Exception
  {
    String test = "if a or b then Accept else Discard";

    PredicateLexer lexer = new PredicateLexer( new ANTLRStringStream( test ) );

    CommonTokenStream stream = new CommonTokenStream( lexer );
    PredicateParser parser = new PredicateParser( stream );

    PredicateStmt stmt = parser.program().value;
    System.out.println( stmt );
  }
}
