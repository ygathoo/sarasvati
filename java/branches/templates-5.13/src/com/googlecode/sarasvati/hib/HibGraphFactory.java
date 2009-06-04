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

    Copyright 2008-2009 Paul Lorenz
*/

package com.googlecode.sarasvati.hib;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hibernate.Hibernate;
import org.hibernate.Session;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.ArcTokenSetMember;
import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.Env;
import com.googlecode.sarasvati.ExecutionType;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.JoinType;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.NodeTokenSetMember;
import com.googlecode.sarasvati.TokenSet;
import com.googlecode.sarasvati.load.AbstractGraphFactory;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.load.NodeFactory;

public class HibGraphFactory extends AbstractGraphFactory<HibGraph>
{
  private Session session;

  HibGraphFactory (final Session session)
  {
    super( HibNode.class );
    this.session = session;
  }

  @Override
  public HibGraph newGraph (final String name, final int version)
  {
    HibGraph newGraph = new HibGraph( name, version );
    session.save( newGraph );
    return newGraph;
  }

  @Override
  public HibArc newArc (final HibGraph graph,
                        final Node startNode,
                        final Node endNode,
                        final String name)
  {
    HibArc arc = new HibArc( graph, startNode, endNode, name );
    session.save( arc );
    graph.getArcs().add( arc );
    return arc;
  }

  @Override
  public Node newNode (final HibGraph graph,
                       final String name,
                       final String type,
                       final JoinType joinType,
                       final boolean isStart,
                       final String guard,
                       final List<Object> customList)
    throws LoadException
  {
    NodeFactory nodeFactory = getNodeFactory( type );
    Node newNode = nodeFactory.newNode( type );

    HibNode node = null;
    HibCustomNodeWrapper customNodeWrapper = null;

    if ( newNode instanceof CustomNode )
    {
      customNodeWrapper = new HibCustomNodeWrapper( (CustomNode)newNode );
      node = customNodeWrapper;
    }
    else
    {
      node = (HibNode)newNode;
    }

    node.setGraph( graph );
    node.setName( name );
    node.setType( type );
    node.setJoinType( joinType );
    node.setStart( isStart );
    node.setGuard( guard );

    if ( customList != null )
    {
      for ( Object custom : customList )
      {
        Map<String, String> customProps = nodeFactory.loadCustom( newNode, custom );

        // If this is a custom node, we need save the properties in the CustomNodeWrapper
        // as well as in the CustomNode, so that they can be set back in when the CustomNode
        // is re-created, after being loaded from the database
        if ( customNodeWrapper != null )
        {
          customNodeWrapper.importProperties( customProps );
        }
      }
    }

    node.create( session );

    HibNodeRef nodeRef = new HibNodeRef( graph, node, "" );
    session.save( nodeRef );
    graph.getNodes().add( nodeRef );

    return nodeRef;
  }

  @Override
  public HibArcToken newArcToken (final GraphProcess process,
                                  final Arc arc,
                                  final ExecutionType executionType,
                                  final NodeToken previousToken)
  {
    HibGraphProcess hibProcess = (HibGraphProcess)process;
    Hibernate.initialize( hibProcess.getExecutionQueue() );
    HibArcToken token = new HibArcToken( hibProcess, (HibArc)arc, executionType, (HibNodeToken)previousToken );
    session.save( token );
    return token;
  }

  @Override
  public Node importNode (final HibGraph graph,
                          final Node node,
                          final String instanceName)
  {
    HibNodeRef nodeRef = (HibNodeRef)node;

    String label = getInstance( nodeRef.getInstance(), instanceName );

    HibNodeRef newRef = new HibNodeRef( graph, nodeRef.getNode(), label );
    session.save( newRef );
    graph.getNodes().add( newRef );

    return newRef;
  }

  @Override
  public HibNodeToken newNodeToken (final GraphProcess process,
                                    final Node node,
                                    final ExecutionType executionType,
                                    final List<ArcToken> parents,
                                    final NodeToken envSource)
  {
    // Here we setup the token attributes for the new node
    // If the node has no predecessors, it will have no attributes
    // If it has only one processor (or only one processor with attributes)
    // it will inherit the attributes of that one node
    // Otherwise, the attributes of all predecessor nodes will get merged into
    // a single set.
    List<ArcToken> envParents = envSource == null ? parents : envSource.getParentTokens();

    HibNodeToken attrSetToken = null;
    Map<String,String> attrMap = new HashMap<String,String>();
    Map<String,Object> transientAttributes = new HashMap<String, Object>();
    boolean isMerge = false;

    for ( ArcToken arcToken : envParents )
    {
      HibNodeToken parent = (HibNodeToken)arcToken.getParentToken();

      if ( parent.getAttrSetToken() == null )
      {
        continue;
      }
      if ( attrSetToken == null )
      {
        attrSetToken = parent.getAttrSetToken();
      }
      else if ( !isMerge )
      {
        attrMap.putAll( attrSetToken.getAttrMap() );
        isMerge = true;
      }

      if ( isMerge )
      {
        attrMap.putAll( parent.getAttrMap() );
      }

      Env mergeEnv = parent.getEnv();
      for ( String name : mergeEnv.getTransientAttributeNames() )
      {
        transientAttributes.put( name, mergeEnv.getTransientAttribute( name ) );
      }
    }

    HibNodeToken token = new HibNodeToken( (HibGraphProcess)process,
                                           (HibNodeRef)node,
                                           attrSetToken,
                                           executionType,
                                           attrMap,
                                           parents,
                                           transientAttributes);
    session.save( token );
    return token;
  }

  @Override
  public HibGraphProcess newProcess (final Graph graph)
  {
    HibGraphProcess process = new HibGraphProcess( (HibGraph)graph);
    session.save( process );
    return process;
  }

  @Override
  public GraphProcess newNestedProcess (final Graph graph,
                                        final NodeToken parentToken)
  {
    HibGraphProcess process = new HibGraphProcess( (HibGraph)graph);
    process.setParentToken( parentToken );
    session.save( process );
    return process;
  }

  @Override
  public TokenSet newTokenSet (final GraphProcess process,
                               final String name)
  {
    HibTokenSet tokenSet = new HibTokenSet( (HibGraphProcess)process, name );
    session.save( tokenSet );
    return tokenSet;
  }

  @Override
  public ArcTokenSetMember newArcTokenSetMember (final TokenSet tokenSet,
                                                 final ArcToken token,
                                                 final int memberIndex)
  {
    HibArcTokenSetMember setMember =
      new HibArcTokenSetMember( (HibTokenSet)tokenSet, (HibArcToken)token, memberIndex );

    session.save( setMember );
    return setMember;
  }

  @Override
  public NodeTokenSetMember newNodeTokenSetMember (final TokenSet tokenSet,
                                                   final NodeToken token,
                                                   final int memberIndex)
  {
    HibNodeTokenSetMember setMember =
      new HibNodeTokenSetMember( (HibTokenSet)tokenSet, (HibNodeToken)token, memberIndex );

    session.save( setMember );
    return setMember;
  }
}