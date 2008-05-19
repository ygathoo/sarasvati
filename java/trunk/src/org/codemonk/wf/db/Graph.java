/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import java.util.List;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.codemonk.wf.IArc;
import org.codemonk.wf.IGraph;
import org.codemonk.wf.INode;

@Entity
@Table (name="wf_graph")
public class Graph implements IGraph
{
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  protected Long   id;
  protected String name;
  protected int    version;

  public Long getId ()
  {
    return id;
  }

  public void setId (Long id)
  {
    this.id = id;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public int getVersion ()
  {
    return version;
  }

  public void setVersion (int version)
  {
    this.version = version;
  }

  @Override
  public List<IArc> getInputArcs (INode node)
  {
    return null;
  }

  @Override
  public List<IArc> getInputArcs (INode node, String arcName)
  {
    return null;
  }

  @Override
  public List<IArc> getOutputArcs (INode node)
  {
    return null;
  }

  @Override
  public List<IArc> getOutputArcs (INode node, String arcName)
  {
    return null;
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
    if ( !( obj instanceof Graph ) ) return false;
    final Graph other = (Graph)obj;
    if ( id == null )
    {
      if ( other.id != null ) return false;
    }
    else if ( !id.equals( other.id ) ) return false;
    return true;
  }
}
