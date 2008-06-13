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

package org.codemonk.wf.mem;

import org.codemonk.wf.Arc;
import org.codemonk.wf.ArcToken;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.Process;

public class MemArcToken implements ArcToken
{
  protected Arc arc;
  protected Process process;
  protected NodeToken parentToken;

  public MemArcToken (Arc arc, Process process, NodeToken parentToken)
  {
    this.arc = arc;
    this.process = process;
    this.parentToken = parentToken;
  }

  @Override
  public Arc getArc()
  {
    return arc;
  }

  @Override
  public Process getProcess()
  {
    return process;
  }

  @Override
  public NodeToken getParentToken ()
  {
    return parentToken;
  }

  @Override
  public void markComplete()
  {
    /* Does nothing */
  }
}