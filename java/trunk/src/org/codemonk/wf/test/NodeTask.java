package org.codemonk.wf.test;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.codemonk.wf.Engine;
import org.codemonk.wf.INodeToken;
import org.codemonk.wf.IProcess;
import org.codemonk.wf.db.HibernateEngine;
import org.codemonk.wf.db.Node;
import org.codemonk.wf.db.NodeToken;
import org.hibernate.Session;

@Entity
@Table (name = "wf_node_task")
public class NodeTask extends Node
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
  public void execute (Engine engine, IProcess process, INodeToken token)
  {
    HibernateEngine hibEngine = (HibernateEngine)engine;

    Session session = hibEngine.getSession();

    TaskState open = (TaskState)session.load( TaskState.class, 0 );
    Task newTask = new Task( (NodeToken)token, getTaskName(), getTaskDesc(), open );
    session.save( newTask );
  }
}