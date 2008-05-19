/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf.db;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.codemonk.wf.Engine;
import org.codemonk.wf.GuardResponse;
import org.codemonk.wf.INode;
import org.codemonk.wf.INodeToken;
import org.codemonk.wf.WfRun;

@Entity
@Table (name="wf_node")
public class Node implements INode
{
  @Id
  @GeneratedValue(strategy=GenerationType.AUTO)
  protected Long   id;
  protected String name;
  protected String type;

  protected boolean join;

  @Override
  public void execute (Engine engine, WfRun wfRun, INodeToken token)
  {
    engine.completeExecution( wfRun, token, "" );
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

  @Override
  public String getType ()
  {
    return type;
  }

  public void setType (String type)
  {
    this.type = type;
  }

  @Override
  public GuardResponse guard (WfRun wfRun, INodeToken token)
  {
    return GuardResponse.AcceptToken;
  }

  @Override
  public boolean isJoin ()
  {
    return false;
  }
}