/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
/**
 * Created on Apr 25, 2008
 */
package com.googlecode.sarasvati.hib;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.googlecode.sarasvati.Arc;

@Entity
@Table (name="wf_arc")
public class HibArc implements Arc
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long id;
  protected String name;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="graph_id")
  protected HibGraph graph;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="a_node_ref_id")
  protected HibNodeRef startNode;

  @ManyToOne (fetch=FetchType.EAGER)
  @JoinColumn (name="z_node_ref_id")
  protected HibNodeRef endNode;

  protected HibArc () { /* Default constructor for hibernate */ }

  protected HibArc (HibGraph graph, HibNodeRef startNode, HibNodeRef endNode, String name)
  {
    this.graph = graph;
    this.startNode = startNode;
    this.endNode = endNode;
    this.name = name;
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
  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public HibGraph getGraph ()
  {
    return graph;
  }

  public void setGraph (HibGraph graph)
  {
    this.graph = graph;
  }

  @Override
  public HibNodeRef getStartNode ()
  {
    return startNode;
  }

  public void setStartNode (HibNodeRef startNode)
  {
    this.startNode = startNode;
  }

  @Override
  public HibNodeRef getEndNode ()
  {
    return endNode;
  }

  public void setEndNode (HibNodeRef endNode)
  {
    this.endNode = endNode;
  }

  @Override
  public String toString ()
  {
    return "[Arc id=" + id + " name=" + name + "]";
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
    if ( !( obj instanceof HibArc ) ) return false;
    final HibArc other = (HibArc)obj;
    if ( id == null )
    {
      if ( other.getId() != null ) return false;
    }
    else if ( !id.equals( other.getId() ) ) return false;
    return true;
  }
}