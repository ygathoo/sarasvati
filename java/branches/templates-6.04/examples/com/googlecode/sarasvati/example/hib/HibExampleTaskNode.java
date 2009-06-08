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
package com.googlecode.sarasvati.example.hib;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.PrimaryKeyJoinColumn;
import javax.persistence.SecondaryTable;

import org.hibernate.Session;

import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.hib.HibNode;
import com.googlecode.sarasvati.hib.HibNodeToken;
import com.googlecode.sarasvati.example.TaskState;

@Entity
@DiscriminatorValue( "task" )
@SecondaryTable( name="wf_node_task", pkJoinColumns=@PrimaryKeyJoinColumn(name="id"))
public class HibExampleTaskNode extends HibNode
{
  @Column (name="name", table="wf_node_task")
  protected String taskName;

  @Column (name="description", table="wf_node_task")
  protected String taskDesc;

  public HibExampleTaskNode() { /* Default constructor for Hibernate */ }

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
  public void backtrack (Engine engine, NodeToken token)
  {
    HibEngine hibEngine = (HibEngine)engine;
    Task task = TaskDAO.getTaskForToken( hibEngine.getSession(), token );
    task.setState( TaskState.Cancelled );
  }

  @SuppressWarnings("unchecked")
  @Override
  public <T> T getAdaptor (Class<T> clazz)
  {
    if ( String.class == clazz )
    {
      return (T)getTaskName();
    }
    return super.getAdaptor( clazz );
  }

  @Override
  public void execute (Engine engine, NodeToken token)
  {
    HibEngine hibEngine = (HibEngine)engine;

    Session session = hibEngine.getSession();

    Task newTask = new Task( (HibNodeToken)token, getTaskName(), getTaskDesc(), TaskState.Open );
    session.save( newTask );

    Env env = token.getEnv();
    env.setAttribute( newTask.getName(), env.getAttribute( newTask.getName(), Integer.class ) + 1 );

    env = token.getProcess().getEnv();
    env.setAttribute( newTask.getName(), env.getAttribute( newTask.getName(), Integer.class ) + 1 );
  }
}