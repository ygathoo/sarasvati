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
  protected long id;

  protected Arc arc;
  protected MemProcess process;
  protected NodeToken parentToken;

  public MemArcToken (Arc arc, MemProcess process, NodeToken parentToken)
  {
    this.id = process.nextTokenId();
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

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + (int)( id ^ ( id >>> 32 ) );
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if ( this == obj ) return true;
    if ( obj == null ) return false;
    if ( !( obj instanceof MemArcToken ) ) return false;
    final MemArcToken other = (MemArcToken)obj;
    if ( id != other.id ) return false;
    return true;
  }
}