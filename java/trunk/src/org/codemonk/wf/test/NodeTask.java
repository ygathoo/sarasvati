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

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.codemonk.wf.Engine;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.Process;
import org.codemonk.wf.db.HibEngine;
import org.codemonk.wf.db.HibNode;
import org.codemonk.wf.db.HibNodeToken;
import org.hibernate.Session;

@Entity
@Table (name = "wf_node_task")
public class NodeTask extends HibNode
{
  @Column (name = "name")
  protected String taskName;

  @Column (name = "description")
  protected String taskDesc;

  public String getTaskName ()
  {
    return taskName;
  }

  public void setTaskName (String taskName)
  {
    this.taskName = taskName;
  }

  public String getTaskDesc ()
  {
    return taskDesc;
  }

  public void setTaskDesc (String taskDesc)
  {
    this.taskDesc = taskDesc;
  }

  @Override
  public String getLabel ()
  {
    return taskName;
  }

  @Override
  public void execute (Engine engine, Process process, NodeToken token)
  {
    HibEngine hibEngine = (HibEngine)engine;

    Session session = hibEngine.getSession();

    TaskState open = (TaskState)session.load( TaskState.class, 0 );
    Task newTask = new Task( (HibNodeToken)token, getTaskName(), getTaskDesc(), open );
    session.save( newTask );

    token.setLongAttribute( newTask.getName(), token.getLongAttribute( newTask.getName() ) + 1 );
  }
}