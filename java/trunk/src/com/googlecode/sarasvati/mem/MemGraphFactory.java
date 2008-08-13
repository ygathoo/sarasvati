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
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphFactory;
import com.googlecode.sarasvati.ImportException;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.Process;

public class MemGraphFactory implements GraphFactory
{
  public static final MemGraphFactory INSTANCE = new MemGraphFactory();

  @Override
  public MemGraph newGraph (String name, int version)
  {
    MemGraph graph = new MemGraph( name );
    MemGraphRepository.INSTANCE.addGraph( graph );
    return graph;
  }

  @Override
  public Arc createArc (Graph graph, Node startNode, Node endNode, String name)
      throws ImportException
  {
    MemArc arc = new MemArc( name, startNode, endNode );
    ((MemGraph)graph).getArcs().add( arc );
    return arc;
  }

  @Override
  public Node importNode (Graph graph, Node node, String instanceName)
  {
    MemNode memNode = (MemNode)node;
    MemNode newNode = memNode.clone();
    newNode.setGraph( graph );
    newNode.setExternal( true );

    MemGraph memGraph = (MemGraph)graph;
    memGraph.getNodes().add( newNode );

    return newNode;
  }

  @Override
  public ArcToken newArcToken (Process process, Arc arc, NodeToken parent)
  {
    return new MemArcToken( arc, process, parent );
  }

  @Override
  public NodeToken newNodeToken (Process process, Node node, List<ArcToken> parents)
  {
    MemNodeToken token = new MemNodeToken( node, process );
    Env env = token.getEnv();

    for ( ArcToken t : parents )
    {
      env.importEnv( t.getParentToken().getEnv() );
    }

    return token;
  }

  @Override
  public Process newProcess (Graph graph)
  {
    return new MemProcess( graph );
  }
}
