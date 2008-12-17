package com.googlecode.sarasvati;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import junit.framework.Assert;

/**
 *
 * Format:
 *   Each line should contain 1 token
 *   The arc tokens should be indented under the node token
 *   that spawned them.
 *
 *  Node token format
 *  [id nodeName status executionType ]
 *
 *  status is one of I = Incomplete or C = Complete
 *  executionType is one of F, B, FB, BB
 *
 *  Arc token format
 *  (status executionType childTokenId/nodeName)
 *
 *  status is one of P - Pending, I - incomplete, C - complete
 *  if status is P or I then the last element will be a node name
 *  if status is C then the last element will be a node token id
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
  public static void validate (GraphProcess p, String state)
  {
    try
    {
      new TestProcess( p.getGraph(), state ).compare( p );
    }
    catch( IOException ioe )
    {
      throw new RuntimeException( ioe );
    }
  }

  protected List<TestNodeToken> startTestTokens = new LinkedList<TestNodeToken>();
  protected Graph graph;

  public TestProcess (Graph graph, String spec) throws IOException
  {
    this.graph = graph;

    BufferedReader reader = new BufferedReader( new StringReader( spec ) );


    Map<String, TestNodeToken> nodeTokens = new HashMap<String, TestNodeToken>();
    Map<String, List<TestArcToken>> arcTokens = new HashMap<String, List<TestArcToken>>();

    TestNodeToken current = null;

    String line = null;
    int lineNumber = 1;
    while ( null != ( line = reader.readLine() ) )
    {
      line = line.trim();
      if ( line.startsWith( "[" ) )
      {
        current = parseNodeToken( line, lineNumber );
        nodeTokens.put( current.getId(), current );
      }
      else if ( line.startsWith( "(" ) )
      {
        parseArcToken(line, current, arcTokens, lineNumber );
      }
      lineNumber++;
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
        token.setChildToken( child );
      }
    }

    for ( TestNodeToken token : nodeTokens.values() )
    {
      if ( token.getParents().isEmpty() )
      {
        startTestTokens.add( token );
      }
    }

    reader.close();
  }

  private TestNodeToken parseNodeToken (String line, int lineNumber)
  {
    line = line.substring( 1, line.length() - 1 );
    String[] parts = line.split( " " );

    boolean complete = false;

    if ( "C".equals( parts[2] ) )
    {
      complete = true;
    }
    else if ( !"I".equals( parts[2] ) )
    {
      throw new RuntimeException( "Unreconized status marker on line number " + lineNumber + " in " + line );
    }

    return new TestNodeToken( lineNumber, parts[0], getNode( parts[1] ), complete, stringToExecutionType( line, parts[3] ) );
  }

  private void parseArcToken (String line, TestNodeToken parent, Map<String, List<TestArcToken>> arcTokens, int lineNumber)
  {
    line = line.substring( 1, line.length() - 1 );
    String[] parts = line.split( " " );

    if ( parent == null )
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
      throw new RuntimeException( "Unreconized status marker '" + parts[0] + "' on line number " + lineNumber + " in " + line );
    }

    TestArcToken token = new TestArcToken( lineNumber, parent, pending, complete, stringToExecutionType( line, parts[1] ) );

    parent.addChild( token );

    if ( complete )
    {
      String childId = parts[2];
      List<TestArcToken> list = arcTokens.get( childId );
      if ( list == null )
      {
        list = new LinkedList<TestArcToken>();
        arcTokens.put( childId, list );
      }
      list.add( token );
    }
    else
    {
      token.setChildNode( getNode( parts[2] ) );
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

  private Node getNode (String name)
  {
    List<Node> nodes = new ArrayList<Node>();
    for ( Node node : graph.getNodes() )
    {
      if ( name.equals( node.getName() ) )
      {
        nodes.add( node );
      }
    }

    if ( nodes.isEmpty() )
    {
      throw new RuntimeException( "Found no nodes named " + name );
    }

    if ( nodes.size() > 1 )
    {
      StringBuilder buf = new StringBuilder();
      for ( Node node : nodes )
      {
        buf.append( "Node id=" );
        buf.append( node.getId() );
        buf.append( " name=" );
        buf.append( node.getName() );
        buf.append( " type=" );
        buf.append( node.getType() );
        buf.append(" external=" );
        buf.append( node.isExternal() );
        buf.append( " join=" );
        buf.append( node.isJoin() );
        buf.append( " start=" );
        buf.append(  node.isStart() );
        buf.append( "\n" );
      }

      throw new RuntimeException( "Found too many nodes named " + name + "\n" + buf.toString() );
    }

    return nodes.get( 0 );
  }

  private LinkedList<NodeToken> getStartTokens (GraphProcess p)
  {
    LinkedList<NodeToken> startTokens = new LinkedList<NodeToken>();

    for ( NodeToken token : p.getNodeTokens() )
    {
      if ( token.getParentTokens().isEmpty() && token.getNode().isStart() )
      {
        startTokens.add( token );
      }
    }

    return startTokens;
  }

  public void compare (GraphProcess p)
  {
    LinkedList<NodeToken> startNodeTokens = getStartTokens( p );

    while ( !startNodeTokens.isEmpty() )
    {
      NodeToken t = startNodeTokens.removeFirst();
      boolean found = true;
      for ( TestNodeToken tt : startTestTokens )
      {
        if ( tt.getNode().equals( t.getNode() ) )
        {
          tt.setToken( t );
          found = true;
          break;
        }
      }

      Assert.assertTrue( "No test node token found for node token: " + t, found );
    }

    for ( TestNodeToken token : startTestTokens )
    {
      Assert.assertNotNull( "No real token found for test token " + token.getId(),  token.getToken() );
    }

    associateTokens();
    validate();
  }

  private void associateTokens ()
  {
    LinkedList<TestNodeToken> queue = new LinkedList<TestNodeToken>();
    queue.addAll( startTestTokens );

    while ( !queue.isEmpty() )
    {
      TestNodeToken testNodeToken = queue.removeFirst();
      NodeToken nodeToken = testNodeToken.getToken();

      for ( ArcToken arcToken : nodeToken.getChildTokens() )
      {
        boolean found = false;
        if ( arcToken.isComplete() )
        {
          NodeToken child = arcToken.getChildToken();
          for ( TestArcToken testArcToken : testNodeToken.getChildren() )
          {
            if ( testArcToken.isComplete() && child.getNode().equals( testArcToken.getChildToken().getNode() ) )
            {
              TestNodeToken testChild = testArcToken.getChildToken();
              Assert.assertNull( "Two arc tokens corresponding to test arc token: " + testArcToken, testArcToken.getToken() );
              testArcToken.setToken( arcToken );

              // If this test node token already processed by some incoming arc, don't re-queue
              if ( testChild.getToken() == null )
              {
                testChild.setToken( child );
                queue.add( testChild );
              }

              found = true;
              break;
            }
          }
        }
        else
        {
          for ( TestArcToken testArcToken : testNodeToken.getChildren() )
          {
            if ( !testArcToken.isComplete() && arcToken.getArc().getEndNode().equals( testArcToken.getChildNode() ) )
            {
              testArcToken.setToken( arcToken );
              found = true;
              break;
            }
          }
        }

        Assert.assertTrue( "No corresponding test arc token found for arc token: " + arcToken, found );
      }

      for ( TestArcToken testArcToken : testNodeToken.getChildren() )
      {
        Assert.assertNotNull( "No arc token found for token " + testArcToken, testArcToken.getToken() );
      }
    }
  }

  private void validate ()
  {
    LinkedList<TestNodeToken> queue = new LinkedList<TestNodeToken>();
    queue.addAll( startTestTokens );

    while ( !queue.isEmpty() )
    {
      TestNodeToken testNodeToken = queue.removeFirst();
      testNodeToken.validate();

      for ( TestArcToken testArcToken : testNodeToken.getChildren() )
      {
        testArcToken.validate();
        if ( testArcToken.getChildToken() != null && !testArcToken.getChildToken().isValidated() )
        {
          queue.add( testArcToken.getChildToken() );
        }
      }
    }
  }
}
