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
package com.googlecode.sarasvati.rubric.lang;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.RecognizerSharedState;
import org.antlr.runtime.TokenStream;

import com.googlecode.sarasvati.rubric.RubricException;

public class ErrorReportingRubricParser extends RubricParser
{
  protected RubricException e;

  public ErrorReportingRubricParser (TokenStream input, RecognizerSharedState state)
  {
    super( input, state );
  }

  public ErrorReportingRubricParser (TokenStream input)
  {
    super( input );
  }

  public RubricException getError ()
  {
    return e;
  }

  @Override
  public String getErrorMessage (RecognitionException re, String[] arg1)
  {
    String message = super.getErrorMessage( re, arg1 );

    message += " at line " + re.line + ", character " + re.charPositionInLine;

    RubricException ne = new RubricException( re, message );

    if (e == null)
    {
      e = ne;
    }
    else
    {
      Throwable curr = e;
      while (curr.getCause() != null)
      {
        curr = curr.getCause();
      }
      curr.initCause( ne );
    }

    return message;
  }

  @Override
  public void emitErrorMessage( String arg0 )
  {
    // do nothing
  }
}
