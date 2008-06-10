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

import javax.persistence.CascadeType;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.OneToOne;
import javax.persistence.PrimaryKeyJoinColumn;

import org.codemonk.wf.WfEngine;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.hib.HibWfEngine;
import org.codemonk.wf.hib.HibNode;
import org.codemonk.wf.hib.HibNodeToken;
import org.hibernate.Session;

@Entity
@DiscriminatorValue( "task" )
public class NodeTask extends HibNode
{
  @OneToOne (cascade = { CascadeType.REMOVE } )
  @PrimaryKeyJoinColumn
  protected NodeTaskDetail detail;

  protected NodeTask() { /* Default constructor for Hibernate */ }

  public NodeTask( HibNode other )
  {
    super( other );
    detail = new NodeTaskDetail ();
  }

  public NodeTaskDetail getDetail ()
  {
    return detail;
  }

  public void setDetail (NodeTaskDetail detail)
  {
    this.detail = detail;
  }

  public String getTaskName ()
  {
    return detail.getTaskName();
  }

  public String getTaskDesc ()
  {
    return detail.getTaskDesc();
  }

  @Override
  public String getLabel ()
  {
    return getTaskName();
  }

  @Override
  public void execute (WfEngine engine, NodeToken token)
  {
    HibWfEngine hibEngine = (HibWfEngine)engine;

    Session session = hibEngine.getSession();

    TaskState open = (TaskState)session.load( TaskState.class, 0 );
    Task newTask = new Task( (HibNodeToken)token, getTaskName(), getTaskDesc(), open );
    session.save( newTask );

    token.setLongAttribute( newTask.getName(), token.getLongAttribute( newTask.getName() ) + 1 );
  }
}