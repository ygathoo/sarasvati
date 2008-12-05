/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/

package com.googlecode.sarasvati.rubric;

import org.antlr.runtime.ANTLRStringStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.RecognitionException;

import com.googlecode.sarasvati.rubric.env.RubricEnv;
import com.googlecode.sarasvati.rubric.lang.ErrorReportingRubricParser;
import com.googlecode.sarasvati.rubric.lang.RubricLexer;
import com.googlecode.sarasvati.rubric.lang.RubricStmt;

public class RubricInterpreter
{
  public static RubricStmt compile (String rubricStatement)
  {
    RubricLexer lexer = new RubricLexer( new ANTLRStringStream( rubricStatement ) );

    CommonTokenStream stream = new CommonTokenStream( lexer );
    ErrorReportingRubricParser parser = new ErrorReportingRubricParser( stream );

    try
    {
      RubricStmt stmt = parser.program().value;
      RubricException e = parser.getError();
      if ( e != null )
      {
        throw e;
      }

      return stmt;
    }
    catch ( RecognitionException re )
    {
      // this should never happen, as the parser is configured to not throw exceptions.
      System.err.println( "Received unexpected recognition exception" );
      re.printStackTrace();
      return null;
    }
  }

  public static Object eval (String rubricStatement, RubricEnv env)
  {
    RubricStmt stmt = compile( rubricStatement );
    return stmt.eval(  env );
  }
}
