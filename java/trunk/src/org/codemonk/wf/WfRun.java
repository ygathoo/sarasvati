/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

import java.util.List;

public class WfRun
{
  protected IGraph            graph;

  protected List<INodeToken> nodeTokens;
  protected List<IArcToken>  arcTokens;

  public IGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (IGraph graph)
  {
    this.graph = graph;
  }

  public List<INodeToken> getNodeTokens ()
  {
    return nodeTokens;
  }

  public void setNodeTokens (List<INodeToken> nodeTokens)
  {
    this.nodeTokens = nodeTokens;
  }

  public List<IArcToken> getArcTokens ()
  {
    return arcTokens;
  }

  public void setArcTokens (List<IArcToken> arcTokens)
  {
    this.arcTokens = arcTokens;
  }

  public void addArcToken (IArcToken token)
  {
    arcTokens.add( token );
  }

  public void removeArcToken (IArcToken token)
  {
    arcTokens.remove( token );
  }

  public void addNodeToken (INodeToken token)
  {
    nodeTokens.add( token );
  }

  public void removeNodeToken (INodeToken token)
  {
    nodeTokens.remove( token );
  }
}