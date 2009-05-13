package com.googlecode.sarasvati.impl;

import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.JoinResult;

public class CompleteJoinResult implements JoinResult
{
  private final List<ArcToken> tokens;
  
  public CompleteJoinResult (final List<ArcToken> tokens)
  {
    this.tokens = tokens;
  }
  
  @Override
  public List<ArcToken> getArcTokensCompletingJoin ()
  {
    return tokens;
  }

  /**
   * Always returns true.
   * 
   * @see JoinResult#isJoinComplete()
   */
  @Override
  public boolean isJoinComplete ()
  {
    return true;
  }
}
