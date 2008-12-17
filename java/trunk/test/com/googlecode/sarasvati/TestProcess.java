package com.googlecode.sarasvati;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

/**
 *
 * Format:
 *   Each line should contain 1 token
 *   The arc tokens should be indented under the node token
 *   that spawned them.
 *
 *  Node token format
 *  [id name status executionType ]
 *
 *  status is one of I = Incomplete or C = Complete
 *  executionType is one of F, B, FB, BB
 *
 *  Arc token format
 *  (status executionType childTokenId)
 *
 *  status is one of P - Pending, I - incomplete, C - complete
 *
 *  ex: [1 foo C F]
 *        (C F 2)
 *        (C F 3)
 *      [2 bar C F]
 *      [3 baz C F]
 *
 * @author Paul Lorenz
 */
public class TestProcess
{
  protected List<TestNodeToken> startTokens = new LinkedList<TestNodeToken>();
  protected Graph graph;

  public TestProcess (Graph graph, String spec) throws IOException
  {
    this.graph = graph;

    BufferedReader reader = new BufferedReader( new StringReader( spec ) );

    String line = null;

    Map<String, TestNodeToken> nodeTokens = new HashMap<String, TestNodeToken>();
    Map<String, List<TestArcToken>> arcTokens = new HashMap<String, List<TestArcToken>>();

    TestNodeToken current = null;

    while ( null != ( line = reader.readLine() ) )
    {
      line = line.trim();
      if ( line.startsWith( "[" ) )
      {
        line = line.substring( 0, line.length() - 1 );
        String[] parts = line.split( " " );

        boolean complete = false;

        if ( "C".equals( parts[2] ) )
        {
          complete = true;
        }
        else if ( !"I".equals( parts[2] ) )
        {
          throw new RuntimeException( "Unreconized status marker in: " + line );
        }

        Node node = null;

        current = new TestNodeToken( node, complete, stringToExecutionType( line, parts[3] ) );
        nodeTokens.put( parts[0], current );
      }
      else if ( line.startsWith( "(" ) )
      {
        line = line.substring( 0, line.length() - 1 );
        String[] parts = line.split( " " );

        if ( current == null )
        {
          throw new RuntimeException( "Can't define arc token before node token" );
        }

        boolean pending = false;
        boolean complete = false;

        if ( "P".equals( parts[0] ) )
        {
          pending = true;
        }
        else if ( "I".equals( parts[0] ) )
        {
          pending = false;
          complete = false;
        }
        else if ( "C".equals( parts[0] ) )
        {
          pending = false;
          complete = true;
        }
        else
        {
          throw new RuntimeException( "Unreconized status marker in: " + line );
        }

        TestArcToken token = new TestArcToken( current, pending, complete, stringToExecutionType( line, parts[1] ) );
        current.addChild( token );

        String childId = parts[2];
        List<TestArcToken> list = arcTokens.get( childId );
        if ( list == null )
        {
          list = new LinkedList<TestArcToken>();
          arcTokens.put( childId, list );
        }
        list.add( token );
      }
    }

    for ( Entry<String,List<TestArcToken>> entry : arcTokens.entrySet() )
    {
      TestNodeToken child = nodeTokens.get( entry.getKey() );

      if ( child == null )
      {
        throw new RuntimeException( "No node found with id: " + entry.getKey() );
      }

      for ( TestArcToken token : entry.getValue() )
      {
        child.addParent( token );
        token.setChild( child );
      }
    }

    for ( TestNodeToken token : nodeTokens.values() )
    {
      if ( token.getParents().isEmpty() )
      {
        startTokens.add( token );
      }
    }
  }

  private static ExecutionType stringToExecutionType (String line, String type)
  {
    if ( "F".equals( type ) )
    {
      return ExecutionType.Forward;
    }

    if ( "B".equals( type ) )
    {
      return ExecutionType.Backward;
    }

    if ( "FB".equals( type ) )
    {
      return ExecutionType.ForwardBacktracked;
    }

    if ( "BB".equals( type ) )
    {
      return ExecutionType.BackwardBacktracked;
    }

    throw new RuntimeException( "Unrecognized execution type in: " + line );
  }

//  public void compare (GraphProcess p)
//  {
//
//  }
}
