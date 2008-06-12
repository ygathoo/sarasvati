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
package org.codemonk.wf.example.mem;

import java.util.Random;

import org.codemonk.wf.Arc;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.WfEngine;
import org.codemonk.wf.mem.MemNode;

public class NodeInit extends MemNode
{
  public NodeInit (MemNode node)
  {
    super( node );
  }

  @Override
  public void execute (WfEngine engine, NodeToken token)
  {
    long iter = 0;

    if ( token.hasAttribute( "iter" ) )
    {
      iter = token.getLongAttribute( "iter" );
    }

    token.setLongAttribute( "iter", ++iter );
    token.setLongAttribute( "rand", ( new Random().nextInt() % 2 ) + 1 );
    engine.completeExecuteNode( token, Arc.DEFAULT_ARC );
  }
}