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

package com.googlecode.sarasvati.mem;

import java.util.Date;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.ExecutionType;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.visitor.TokenVisitor;

public class MemArcToken implements ArcToken
{
  protected Arc arc;
  protected GraphProcess process;
  protected NodeToken parentToken;
  protected NodeToken childToken;
  protected boolean pending;
  protected Date completeDate;
  protected ExecutionType executionType;

  public MemArcToken (Arc arc, GraphProcess process, ExecutionType executionType, NodeToken parentToken)
  {
    this.arc = arc;
    this.process = process;
    this.executionType = executionType;
    this.parentToken = parentToken;
    this.pending = true;
  }

  @Override
  public Arc getArc ()
  {
    return arc;
  }

  @Override
  public GraphProcess getProcess ()
  {
    return process;
  }

  @Override
  public NodeToken getParentToken ()
  {
    return parentToken;
  }

  @Override
  public NodeToken getChildToken()
  {
    return childToken;
  }

  @Override
  public boolean isComplete ()
  {
    return completeDate != null;
  }

  @Override
  public void markComplete (Engine engine, NodeToken token)
  {
    this.completeDate = new Date();
    this.childToken = token;
  }

  @Override
  public boolean isPending ()
  {
    return pending;
  }

  @Override
  public void markProcessed (Engine engine)
  {
    this.pending = false;
  }

  @Override
  public ExecutionType getExecutionType ()
  {
    return executionType;
  }

  @Override
  public void markBacktracked (Engine engine)
  {
    executionType = executionType.getCorrespondingBacktracked();
  }

  @Override
  public void accept (TokenVisitor visitor)
  {
    visitor.visit( this );
  }
}