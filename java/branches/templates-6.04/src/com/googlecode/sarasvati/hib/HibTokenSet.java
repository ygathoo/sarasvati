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

    Copyright 2009 Paul Lorenz
*/

package com.googlecode.sarasvati.hib;

import java.util.LinkedList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.Cascade;
import org.hibernate.annotations.Type;
import org.hibernate.annotations.Where;

import com.googlecode.sarasvati.ArcTokenSetMember;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeTokenSetMember;
import com.googlecode.sarasvati.TokenSet;

@Entity
@Table(name="wf_token_set")
public class HibTokenSet implements TokenSet
{
  @Id
  @GeneratedValue(strategy=GenerationType.IDENTITY)
  protected Long    id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "process_id")
  protected HibGraphProcess process;

  @Column(name="name")
  protected String name;

  @Type (type="yes_no")
  @Column( name="complete")
  protected boolean complete = false;

  @OneToMany (mappedBy="tokenSet", targetEntity=HibArcTokenSetMember.class, fetch=FetchType.LAZY, cascade=CascadeType.REMOVE)
  @Cascade( org.hibernate.annotations.CascadeType.LOCK )
  @Where( clause="token.completeDate is null" )
  protected List<ArcTokenSetMember> activeArcTokenSetMembers = new LinkedList<ArcTokenSetMember>();

  @OneToMany (mappedBy="tokenSet", targetEntity=HibNodeTokenSetMember.class, fetch=FetchType.LAZY)
  @Cascade( org.hibernate.annotations.CascadeType.LOCK )
  @Where( clause="token.completeDate is null" )
  protected List<NodeTokenSetMember> activeNodeTokenSetMembers = new LinkedList<NodeTokenSetMember>();

  public HibTokenSet (final HibGraphProcess process,
                      final String name)
  {
    this.process = process;
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

  public void setName (final String name)
  {
    this.name = name;
  }

  @Override
  public HibGraphProcess getProcess ()
  {
    return process;
  }

  public void setProcess (final HibGraphProcess process)
  {
    this.process = process;
  }

  @Override
  public List<ArcTokenSetMember> getActiveArcTokenSetMembers ()
  {
    return activeArcTokenSetMembers;
  }

  @Override
  public List<NodeTokenSetMember> getActiveNodeTokenSetMembers ()
  {
    return activeNodeTokenSetMembers;
  }

  @Override
  public boolean isComplete ()
  {
    return complete;
  }

  public void setComplete (boolean complete)
  {
    this.complete = complete;
  }

  @Override
  public void markComplete (Engine engine)
  {
    complete = true;
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
    if (!(obj instanceof HibTokenSet))
      return false;
    HibTokenSet other = (HibTokenSet) obj;
    if (id == null)
    {
      if (other.getId() != null)
        return false;
    } else if (!id.equals( other.getId() ))
      return false;
    return true;
  }
}