package com.googlecode.sarasvati.impl;

import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.TokenSetMemberEnv;

public class TokenSetMemberEnvAdapter implements Env
{
  private final TokenSetMemberEnv tokenSetEnv;
  private final int memberIndex;

  public TokenSetMemberEnvAdapter (final TokenSetMemberEnv tokenSetEnv,
                                   final int memberIndex)
  {
    this.tokenSetEnv = tokenSetEnv;
    this.memberIndex = memberIndex;
  }

  @Override
  public Iterable<String> getAttributeNames ()
  {
    return tokenSetEnv.getAttributeNames( memberIndex );
  }

  @Override
  public String getAttribute (final String name)
  {
    return tokenSetEnv.getAttribute( memberIndex, name );
  }

  @Override
  public <T> T getAttribute (final String name,
                             final Class<T> type)
  {
    return tokenSetEnv.getAttribute( memberIndex, name, type );
  }

  @Override
  public void setAttribute (final String name,
                            final String value)
  {
    tokenSetEnv.setAttribute( memberIndex, name, value );
  }

  @Override
  public void setAttribute (final String name,
                            final Object value)
  {
    tokenSetEnv.setAttribute( memberIndex, name, value );
  }

  @Override
  public boolean hasAttribute (final String name)
  {
    return tokenSetEnv.hasAttribute( memberIndex, name );
  }

  @Override
  public void importEnv (final Env env)
  {
    for ( String name : env.getAttributeNames() )
    {
      setAttribute( name, env.getAttribute( name ) );
    }

    for ( String name : env.getTransientAttributeNames() )
    {
      setTransientAttribute( name, env.getTransientAttribute( name ) );
    }
  }

  @Override
  public void removeAttribute (final String name)
  {
    tokenSetEnv.removeAttribute( memberIndex, name );
  }

  @Override
  public Object getTransientAttribute (final String name)
  {
    return tokenSetEnv.getTransientAttribute( memberIndex, name );
  }

  @Override
  public Iterable<String> getTransientAttributeNames ()
  {
    return tokenSetEnv.getTransientAttributeNames( memberIndex );
  }

  @Override
  public boolean hasTransientAttribute (final String name)
  {
    return tokenSetEnv.hasTransientAttribute( memberIndex, name );
  }

  @Override
  public void removeTransientAttribute (final String name)
  {
    tokenSetEnv.removeTransientAttribute( memberIndex, name );
  }

  @Override
  public void setTransientAttribute (final String name,
                                     final Object value)
  {
    tokenSetEnv.setTransientAttribute( memberIndex, name, value );
  }
}