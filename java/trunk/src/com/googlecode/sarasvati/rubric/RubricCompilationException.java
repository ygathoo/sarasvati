package com.googlecode.sarasvati.rubric;

import org.antlr.runtime.RecognitionException;

public class RubricCompilationException extends RubricException
{
  private static final long serialVersionUID = 1L;

  protected RecognitionException re;

  public RubricCompilationException (RecognitionException re, String message)
  {
    super( message );
    this.re = re;
  }

  @Override
  public RecognitionException getRecognitionException ()
  {
    return re;
  }
}
