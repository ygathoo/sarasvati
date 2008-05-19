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