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

import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.NonRecursiveEngine;
import com.googlecode.sarasvati.Process;

public class MemEngine extends NonRecursiveEngine
{
  @Override
  protected ArcToken newArcToken (Process process, Arc arc, NodeToken parent)
  {
    return new MemArcToken( arc, process, parent );
  }

  @Override
  protected NodeToken newNodeToken (Process process, Node node, List<ArcToken> parents)
  {
    MemNodeToken token = new MemNodeToken( node, process );

    for ( ArcToken t : parents )
    {
      MemNodeToken parentToken = (MemNodeToken)t.getParentToken();
      for ( String name : parentToken.getAttributeNames() )
      {
        token.setStringAttribute( name, parentToken.getStringAttribute( name ) );
      }
    }

    return token;
  }

  @Override
  protected Process newProcess (Graph graph)
  {
    return new MemProcess( graph );
  }
}