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
package com.googlecode.sarasvati.unittest.framework;

import java.io.File;
import java.util.HashSet;
import java.util.Set;

import junit.framework.Assert;

import org.junit.Before;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.event.EventActions;
import com.googlecode.sarasvati.event.ExecutionEvent;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.event.ExecutionListener;
import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.mem.MemEngine;
import com.googlecode.sarasvati.mem.MemGraph;
import com.googlecode.sarasvati.xml.XmlLoader;

public class ExecutionTest
{
  public static class DuplicateEventDetector implements ExecutionListener
  {
    private Set<NodeToken> backtrackedNodeTokens = new HashSet<NodeToken>();
    private Set<ArcToken> backtrackedArcTokens = new HashSet<ArcToken>();

    private Set<NodeToken> completedNodeTokens = new HashSet<NodeToken>();
    private Set<ArcToken> completedArcTokens = new HashSet<ArcToken>();


    @Override
    public EventActions notify (final ExecutionEvent event)
    {
      if ( event.getEventType() == ExecutionEventType.NODE_TOKEN_BACKTRACKED )
      {
        if ( backtrackedNodeTokens.contains( event.getNodeToken() ) )
        {
          throw new RuntimeException( "Node Token " + event.getNodeToken() + " notified of backtracking twice" );
        }
        backtrackedNodeTokens.add( event.getNodeToken() );
      }

      if ( event.getEventType() == ExecutionEventType.ARC_TOKEN_BACKTRACKED )
      {
        if ( backtrackedArcTokens.contains( event.getArcToken() ) )
        {
          throw new RuntimeException( "Arc Token " + event.getNodeToken() + " notified of backtracking twice" );
        }
        backtrackedArcTokens.add( event.getArcToken() );
      }

      if ( event.getEventType() == ExecutionEventType.NODE_TOKEN_COMPLETED )
      {
        if ( completedNodeTokens.contains( event.getNodeToken() ) )
        {
          throw new RuntimeException( "Node Token " + event.getNodeToken() + " notified of completion twice" );
        }
        completedNodeTokens.add( event.getNodeToken() );
      }

      if ( event.getEventType() == ExecutionEventType.ARC_TOKEN_COMPLETED )
      {
        if ( completedArcTokens.contains( event.getArcToken() ) )
        {
          throw new RuntimeException( "Arc Token " + event.getNodeToken() + " notified of completion twice" );
        }
        completedArcTokens.add( event.getArcToken() );
      }

      return null;
    }
  }

  protected MemEngine engine;

  @Before
  public void setup ()
  {
    engine = new MemEngine();
    engine.addExecutionListener( DuplicateEventDetector.class, ExecutionEventType.values());
  }

  protected Graph ensureLoaded (final String name) throws Exception
  {
    File basePath = new File( "common/unit-test/" );
    assert basePath.exists();
    GraphLoader<MemGraph> loader = engine.getLoader();

    if ( !loader.isLoaded( name ) )
    {
      loader.loadDefinition( new XmlLoader(), new File( basePath, name + ".wf.xml" ) );
      // OR:
      //loader.loadDefinition( new XmlLoader().translate( new File( basePath, name + ".wf.xml" ) ) );
    }
    return engine.getRepository().getLatestGraph( name );
  }

  public NodeToken getActiveToken (final GraphProcess p, final String nodeName)
  {
    for ( NodeToken token : p.getActiveNodeTokens() )
    {
      if ( nodeName.equals( token.getNode().getName() ) )
      {
        return token;
      }
    }

    Assert.assertTrue( "No node token found on node: " + nodeName, false );
    return null;
  }

  public void completeToken (final GraphProcess p, final String nodeName)
  {
    engine.complete( getActiveToken( p, nodeName ), Arc.DEFAULT_ARC );
  }

  public void completeToken (final GraphProcess p, final String nodeName, final String arcName)
  {
    engine.complete( getActiveToken( p, nodeName ), arcName );
  }
}