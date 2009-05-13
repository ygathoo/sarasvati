/**
 * Created on Apr 27, 2009
 */
package com.googlecode.sarasvati.mem;

import com.googlecode.sarasvati.Token;
import com.googlecode.sarasvati.TokenSet;
import com.googlecode.sarasvati.TokenSetMember;

public class MemTokenSetMember implements TokenSetMember
{
  protected TokenSet tokenSet;
  protected Token token;
  protected int memberIndex;

  public MemTokenSetMember (TokenSet tokenSet, Token token, int memberIndex)
  {
    this.tokenSet = tokenSet;
    this.token = token;
    this.memberIndex = memberIndex;
  }

  @Override
  public TokenSet getTokenSet ()
  {
    return tokenSet;
  }

  @Override
  public Token getToken ()
  {
    return token;
  }

  @Override
  public int getMemberIndex ()
  {
    return memberIndex;
  }
}
