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
package com.googlecode.sarasvati.example.mem;

import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.example.XmlTaskDef;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.mem.MemNode;

public class TaskNode extends MemNode
{
  protected String taskName;
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
  public void execute (Engine engine, NodeToken token)
  {
    Task newTask = new Task( token, getTaskName(), getTaskDesc(), TaskState.Open );
    TaskList.getTasks().add( newTask );

    Env env = token.getEnv();
    env.setLongAttribute( newTask.getName(), env.getLongAttribute( newTask.getName() ) + 1 );

    env = token.getProcess().getEnv();
    env.setLongAttribute( newTask.getName(), env.getLongAttribute( newTask.getName() ) + 1 );
  }

  @Override
  public void loadCustom (Object custom) throws LoadException
  {
    if ( custom == null || !(custom instanceof XmlTaskDef) )
    {
      throw new LoadException( "Task node '" + getName() +
                                 "' in definition of '" + getGraph().getName() +
                                 "' contains no (or improperly specified) task-def element." );
    }

    XmlTaskDef taskDef = (XmlTaskDef)custom;

    taskName = taskDef.getTaskName();
    taskDesc = taskDef.getDescription();
  }
}