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
import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.GuardAction;
import com.googlecode.sarasvati.MapEnv;
import com.googlecode.sarasvati.NestedEnv;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.GraphProcess;

public class MemNodeToken implements NodeToken
{
  protected Node node;
  protected GraphProcess process;
  protected GuardAction guardAction;
  protected Date createDate;
  protected Map<String, String> attributes = new HashMap<String, String>();

  protected Env env = new MapEnv();
  protected Env fullEnv = null;

  public MemNodeToken (Node node, GraphProcess process)
  {
    this.node = node;
    this.process = process;
    this.createDate = new Date();
  }

  @Override
  public Node getNode ()
  {
    return node;
  }

  @Override
  public GraphProcess getProcess ()
  {
    return process;
  }

  @Override
  public GuardAction getGuardAction ()
  {
    return guardAction;
  }


  @Override
  public Date getCreateDate()
  {
    return createDate;
  }

  @Override
  public void recordGuardAction (Engine engine, GuardAction action)
  {
    this.guardAction = action;
  }

  @Override
  public void markComplete (Engine engine)
  {
    /** Does nothing */
  }

  @Override
  public Env getFullEnv()
  {
    if ( fullEnv == null )
    {
      fullEnv = new NestedEnv( env, process.getEnv() );
    }
    return fullEnv;
  }

  @Override
  public Env getEnv()
  {
    return env;
  }
}