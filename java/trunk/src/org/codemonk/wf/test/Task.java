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
package org.codemonk.wf.test;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.codemonk.wf.db.HibNodeRef;
import org.codemonk.wf.db.HibNodeToken;

@Entity
@Table(name = "wf_task")
public class Task
{
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  protected Long      id;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn(name = "node_token_id")
  protected HibNodeToken nodeToken;

  protected String    name;

  protected String    description;

  @ManyToOne(fetch = FetchType.EAGER)
  @JoinColumn (name="state")
  protected TaskState state;

  public Task() { /* Default constructor for Hibernate */ }

  public Task( HibNodeToken nodeToken, String name, String description, TaskState state )
  {
    this.nodeToken = nodeToken;
    this.name = name;
    this.description = description;
    this.state = state;
  }

  public Long getId()
  {
    return id;
  }

  public void setId( Long id )
  {
    this.id = id;
  }

  public HibNodeToken getNodeToken()
  {
    return nodeToken;
  }

  public void setNodeToken( HibNodeToken nodeToken )
  {
    this.nodeToken = nodeToken;
  }

  public String getName()
  {
    return name;
  }

  public void setName( String name )
  {
    this.name = name;
  }

  public String getDescription()
  {
    return description;
  }

  public void setDescription( String description )
  {
    this.description = description;
  }

  public TaskState getState()
  {
    return state;
  }

  public void setState( TaskState state )
  {
    this.state = state;
  }

  public boolean isRejectable ()
  {
    HibNodeRef nodeRef = getNodeToken().getNode();
    return !nodeRef.getGraph().getOutputArcs( nodeRef, "reject" ).isEmpty();
  }

  @Override
  public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((id == null) ? 0 : id.hashCode());
    return result;
  }

  @Override
  public boolean equals( Object obj )
  {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    final Task other = (Task) obj;
    if (id == null)
    {
      if (other.id != null)
        return false;
    } else if (!id.equals( other.id ))
      return false;
    return true;
  }
}