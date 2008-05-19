/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.LinkedList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.codemonk.wf.IArcToken;
import org.codemonk.wf.IProcess;

@Entity
@Table (name="wf_process")
public class Process implements IProcess
{
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  protected Long id;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn( name="graph_id")
  protected Graph            graph;

  @Transient
  protected List<IArcToken>  arcTokens;

  public Process () { /* Default constructor for Hibernate */ }

  public Process (Graph graph)
  {
    this.graph = graph;
    this.arcTokens = new LinkedList<IArcToken>();
  }

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  @Override
  public Graph getGraph ()
  {
    return graph;
  }

  public void setGraph (Graph graph)
  {
    this.graph = graph;
  }

  @Override
  public List<IArcToken> getArcTokens ()
  {
    if ( arcTokens == null )
    {
      // TODO: Load arc tokens
      throw new UnsupportedOperationException( "Not yet implemented: Process.getArcTokens" );
    }

    return arcTokens;
  }

  public void setArcTokens (List<IArcToken> arcTokens)
  {
    this.arcTokens = arcTokens;
  }

  @Override
  public void addArcToken (IArcToken token)
  {
    arcTokens.add( token );
  }

  @Override
  public void removeArcToken (IArcToken token)
  {
    arcTokens.remove( token );
  }

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ( ( id == null )
        ? 0 : id.hashCode() );
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if ( this == obj ) return true;
    if ( obj == null ) return false;
    if ( !( obj instanceof Process ) ) return false;
    final Process other = (Process)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}