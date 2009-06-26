/**
 * Created on Jun 26, 2009
 */
package com.googlecode.sarasvati.editor.model;

public class ExternalState extends GraphMemberState
{
  protected final String graphName;

  public ExternalState (final String name,
                        final String graphName)
  {
    super( name );
    this.graphName = graphName;
  }

  public String getGraphName ()
  {
    return graphName;
  }
}
