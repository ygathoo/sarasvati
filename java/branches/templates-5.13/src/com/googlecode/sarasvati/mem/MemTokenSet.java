/**
 * Created on Apr 27, 2009
 */
package com.googlecode.sarasvati.mem;

import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.TokenSet;

public class MemTokenSet implements TokenSet
{
  protected final GraphProcess process;
  protected final String name;

  public MemTokenSet (final GraphProcess process, final String name)
  {
    this.process = process;
    this.name = name;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  @Override
  public GraphProcess getProcess ()
  {
    return process;
  }
}
