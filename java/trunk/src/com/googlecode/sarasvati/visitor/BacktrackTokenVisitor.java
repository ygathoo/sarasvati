package com.googlecode.sarasvati.visitor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.ExecutionType;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.WorkflowException;

public class BacktrackTokenVisitor implements TokenVisitor
{
  protected Engine engine;
  protected NodeToken destinationToken;

  protected LinkedList<NodeToken> queue = new LinkedList<NodeToken>();
  protected Set<ArcToken> arcTokenLeaves = new HashSet<ArcToken>();

  protected Set<NodeToken> visited = new HashSet<NodeToken>();
  protected Map<ArcToken,ArcToken> arcTokenMap = new HashMap<ArcToken, ArcToken>();

  public BacktrackTokenVisitor (Engine engine, NodeToken destinationToken)
  {
    this.engine = engine;
    this.destinationToken = destinationToken;
  }

  @Override
  public void visit (NodeToken token)
  {
    if ( !token.getNode().isBacktrackable( token ) )
    {
      throw new WorkflowException( "Can not backtrack node name: " +
                                   token.getNode().getName()  +
                                   "id: " + token.getNode().getId() );
    }

    if ( token.getChildTokens().isEmpty() )
    {
      queue.add( token );
    }
    visited.add( token );
  }

  @Override
  public void visit (ArcToken token)
  {
    if ( token.getChildToken() == null )
    {
      arcTokenLeaves.add( token );
    }
  }

  private void backtrackLeafArcTokens ()
  {
    for ( ArcToken token : arcTokenLeaves )
    {
      arcTokenMap.put( token, token );
      token.markBacktracked( engine );
      queue.add( token.getParentToken() );
      token.getProcess().removeActiveArcToken( token );
    }
  }

  public NodeToken backtrack ()
  {
    backtrackLeafArcTokens();

    NodeToken resultToken = null;

    while ( !queue.isEmpty() )
    {
      NodeToken token = queue.removeFirst();
      NodeToken backtrackToken = backtrackNodeToken( token );
      if ( token == destinationToken )
      {
        resultToken = backtrackToken;
      }
      else
      {
        backtrackToken.markBacktracked( engine );
        backtrackToken.markComplete( engine );
      }
    }

    return resultToken;
  }

  private NodeToken backtrackNodeToken (NodeToken token)
  {
    NodeToken backtrackToken = token;

    if ( !token.isComplete() )
    {
      token.markComplete( engine );
      token.markBacktracked( engine );
      token.getProcess().removeActiveNodeToken( token );
    }
    else if ( !token.getExecutionType().isBacktracked() )
    {
      List<ArcToken> parents = new ArrayList<ArcToken>( token.getChildTokens().size() );
      for ( ArcToken childToken : token.getChildTokens() )
      {
        parents.add( arcTokenMap.get( childToken ) );
      }

      backtrackToken =
        engine.getFactory().newNodeToken( token.getProcess(),
                                          token.getNode(),
                                          ExecutionType.Backward,
                                          parents );

      for ( ArcToken parent : parents )
      {
        parent.markComplete( engine, backtrackToken );
      }
    }

    for ( ArcToken parent : token.getParentTokens() )
    {
      boolean backtrackParent = visited.contains( parent.getParentToken() );

      if ( !parent.getExecutionType().isBacktracked() )
      {
        parent.markBacktracked( engine );
        ArcToken backtrackArcToken =
          engine.getFactory().newArcToken( token.getProcess(),
                                           parent.getArc(),
                                           ExecutionType.Backward,
                                           backtrackToken );
        backtrackToken.getChildTokens().add( backtrackArcToken );

        backtrackArcToken.markProcessed( engine );

        if ( backtrackParent )
        {
          backtrackArcToken.markBacktracked( engine );
        }
        arcTokenMap.put( parent, backtrackArcToken );
      }

      if ( backtrackParent )
      {
        queue.add( parent.getParentToken() );
      }
    }

    return backtrackToken;
  }
}