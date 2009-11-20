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

    Copyright 2009 Paul Lorenz
*/
package com.googlecode.sarasvati.join;

import org.antlr.runtime.RecognitionException;
import org.antlr.runtime.RecognizerSharedState;
import org.antlr.runtime.TokenStream;

import com.googlecode.sarasvati.join.lang.JoinLangParser;

public class ErrorReportingJoinLangParser extends JoinLangParser
{
  protected JoinLangException e;

  public ErrorReportingJoinLangParser (final TokenStream input, final RecognizerSharedState state)
  {
    super( input, state );
  }

  public ErrorReportingJoinLangParser (final TokenStream input)
  {
    super( input );
  }

  public JoinLangException getError ()
  {
    return e;
  }

  @Override
  public String getErrorMessage (final RecognitionException re, final String[] arg1)
  {
    String message = super.getErrorMessage( re, arg1 );

    message += " at line " + re.line + ", character " + re.charPositionInLine;

    JoinLangException ne = new JoinLangCompilationException( re, message );

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
  public void emitErrorMessage( final String arg0 )
  {
    // do nothing
  }
}
