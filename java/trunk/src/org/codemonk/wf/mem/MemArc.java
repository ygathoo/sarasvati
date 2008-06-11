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
import org.codemonk.wf.Node;

public class MemArc implements Arc
{
  protected String  name;
  protected MemNode startNode;
  protected MemNode endNode;

  public MemArc (String name, MemNode startNode, MemNode endNode)
  {
    super();
    this.name = name;
    this.startNode = startNode;
    this.endNode = endNode;
  }

  @Override
  public Node getEndNode ()
  {
    return endNode;
  }

  @Override
  public String getName ()
  {
    return name;
  }

  @Override
  public Node getStartNode ()
  {
    return startNode;
  }
}
