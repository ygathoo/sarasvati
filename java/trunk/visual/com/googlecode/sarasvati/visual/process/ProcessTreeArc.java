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

    Copyright 2009 Paul Lorenz
*/
package com.googlecode.sarasvati.visual.process;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;

public class ProcessTreeArc implements VisualProcessArc
{
  protected ArcToken token;
  protected Arc      arc;
  protected ProcessTreeNode parent;
  protected ProcessTreeNode child;

  public ProcessTreeArc(ArcToken token, ProcessTreeNode parent, ProcessTreeNode child)
  {
    this.token  = token;
    this.parent = parent;
    this.child  = child;
  }

  public ProcessTreeArc(Arc arc, ProcessTreeNode parent, ProcessTreeNode child)
  {
    this.arc    = arc;
    this.parent = parent;
    this.child  = child;
  }

  public ArcToken getToken()
  {
    return token;
  }

  public Arc getArc ()
  {
    return token == null ? arc : token.getArc();
  }

  public ProcessTreeNode getParent()
  {
    return parent;
  }

  public ProcessTreeNode getChild()
  {
    return child;
  }
}
