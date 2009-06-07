package com.googlecode.sarasvati.impl;

import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.hib.HibTokenSetEnv;

public class TokenSetMemberEnv implements Env
{
  private int memberIndex;
  private HibTokenSetEnv tokenSetEnv;

  @Override
  public Iterable<String> getAttributeNames ()
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public boolean getBooleanAttribute (String name)
  {
    return tokenSetEnv.getBooleanAttribute( memberIndex, name );
  }

  @Override
  public long getLongAttribute (String name)
  {
    return tokenSetEnv.getLongAttribute( memberIndex, name );
  }

  @Override
  public String getStringAttribute (String name)
  {
    return tokenSetEnv.getStringAttribute( memberIndex, name );
  }

  @Override
  public Object getTransientAttribute (String name)
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public Iterable<String> getTransientAttributeNames ()
  {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public boolean hasAttribute (String name)
  {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public boolean hasTransientAttribute (String name)
  {
    // TODO Auto-generated method stub
    return false;
  }

  @Override
  public void importEnv (Env env)
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void removeAttribute (String name)
  {
    tokenSetEnv.removeAttribute( memberIndex, name );
  }

  @Override
  public void removeTransientAttribute (String name)
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void setBooleanAttribute (String name, boolean value)
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void setLongAttribute (String name, long value)
  {
    // TODO Auto-generated method stub

  }

  @Override
  public void setStringAttribute (String name, String value)
  {
    tokenSetEnv.setStringAttribute( memberIndex, name, value );
  }

  @Override
  public void setTransientAttribute (String name, Object value)
  {
    // TODO Auto-generated method stub

  }
}