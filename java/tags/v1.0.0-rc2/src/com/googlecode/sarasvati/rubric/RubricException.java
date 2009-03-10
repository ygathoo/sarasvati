package com.googlecode.sarasvati.rubric;

import org.antlr.runtime.RecognitionException;

public class RubricException extends RuntimeException
{
  private static final long serialVersionUID = 1L;

  protected RecognitionException re;

  public RubricException (RecognitionException re, String message)
  {
    super( message );
    this.re = re;
  }

  public RecognitionException getRecognitionException ()
  {
    return re;
  }
}
