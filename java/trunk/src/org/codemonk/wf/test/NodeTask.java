package org.codemonk.wf.test;

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
@Table (name="wf_node_task")
public class NodeTask extends Node
{
  protected String name;

  protected String description;

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

  @Override
  public void execute( Engine engine, IProcess process, INodeToken token )
  {
    HibernateEngine hibEngine = (HibernateEngine)engine;

    Session session = hibEngine.getSession();

    TaskState open = (TaskState) session.load( TaskState.class, 0 );
    Task newTask = new Task( (NodeToken)token, getName(), getDescription(), open );
    session.save( newTask );
  }


}
