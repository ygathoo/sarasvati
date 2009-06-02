/**
 * Created on Apr 27, 2009
 */
package com.googlecode.sarasvati.mem;

import java.util.LinkedList;
import java.util.List;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.TokenSet;

public class MemTokenSet implements TokenSet
{
  protected final GraphProcess process;
  protected final String name;

  protected List<ArcToken> activeArcTokens = new LinkedList<ArcToken>();
  protected List<NodeToken> activeNodeTokens = new LinkedList<NodeToken>();

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

  @Override
  public void addArcToken (ArcToken token, int index)
  {
    activeArcTokens.add( token );
  }

  @Override
  public void addNodeToken (NodeToken token, int index)
  {
    activeNodeTokens.add( token );
  }

  @Override
  public List<ArcToken> getActiveArcTokens ()
  {
    return activeArcTokens;
  }

  @Override
  public List<NodeToken> getActiveNodeTokens ()
  {
    return activeNodeTokens;
  }
}