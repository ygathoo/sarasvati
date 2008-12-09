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
