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
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.codemonk.wf.IArcToken;
import org.codemonk.wf.INodeToken;
import org.codemonk.wf.IProcess;
import org.hibernate.annotations.Where;

@Entity
@Table (name="wf_process")
public class HibProcess implements IProcess
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long id;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn( name="graph_id")
  protected HibGraph            graph;

  @OneToMany (mappedBy="process", targetEntity=HibArcToken.class, fetch=FetchType.LAZY)
  @Where (clause="complete_date is null")
  protected List<IArcToken>  arcTokens;

  @OneToMany (mappedBy="process", targetEntity=HibNodeToken.class, fetch=FetchType.LAZY)
  @Where (clause="complete_date is null")
  protected List<INodeToken>  nodeTokens;

  public HibProcess () { /* Default constructor for Hibernate */ }

  public HibProcess (HibGraph graph)
  {
    this.graph = graph;
    this.arcTokens = new LinkedList<IArcToken>();
    this.nodeTokens = new LinkedList<INodeToken>();
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
  public HibGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (HibGraph graph)
  {
    this.graph = graph;
  }

  @Override
  public List<IArcToken> getArcTokens ()
  {
    return arcTokens;
  }

  public void setArcTokens (List<IArcToken> arcTokens)
  {
    this.arcTokens = arcTokens;
  }

  public List<INodeToken> getNodeTokens()
  {
    return nodeTokens;
  }

  public void setNodeTokens( List<INodeToken> nodeTokens )
  {
    this.nodeTokens = nodeTokens;
  }

  @Override
  public void addArcToken (IArcToken token)
  {
    getArcTokens().add( token );
  }

  @Override
  public void removeArcToken (IArcToken token)
  {
    getArcTokens().remove( token );
  }

  @Override
  public void addNodeToken (INodeToken token)
  {
    getNodeTokens().add( token );
  }

  @Override
  public void removeNodeToken (INodeToken token)
  {
    getNodeTokens().remove( token );
  }

  public boolean isComplete ()
  {
    return getArcTokens().isEmpty() && getNodeTokens().isEmpty();
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
    if ( !( obj instanceof HibProcess ) ) return false;
    final HibProcess other = (HibProcess)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}