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
import com.googlecode.sarasvati.GuardAction;
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

  protected BacktrackMirrors backtrackMirror = new BacktrackMirrors();
  protected Map<NodeToken,NodeToken> parentMap = new HashMap<NodeToken, NodeToken>();

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
    if ( token.getExecutionType() != ExecutionType.Forward  )
    {
      backtrackMirror.addVisited( token );
    }

    if ( token.getChildToken() == null )
    {
      if ( token.getExecutionType() == ExecutionType.UTurn )
      {
        ArcToken mirror = backtrackMirror.getMirror( token );
        arcTokenMap.put( mirror, token );
        queue.add( mirror.getParentToken() );
      }
      else
      {
        arcTokenLeaves.add( token );
      }
    }

    if (token.getExecutionType() == ExecutionType.Backtracked)
    {
      ArcToken mirror = backtrackMirror.getMirror( token );
      parentMap.put( token.getChildToken(), mirror.getParentToken() );
    }
  }

  @Override
  public boolean follow (ArcToken child)
  {
    if ( child.getExecutionType() != ExecutionType.Forward )
    {
      backtrackMirror.findMirror( child );
    }

    if ( child.getExecutionType() == ExecutionType.Backtracked ||
         child.getExecutionType() == ExecutionType.UTurn ||
         child.getExecutionType() == ExecutionType.UTurnBacktracked )
    {
      return backtrackMirror.hasMirror( child );
    }

    return true;
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
      if (token.getExecutionType().isBacktracked() )
      {
        continue;
      }

      token.getNode().backtrack( token );

      boolean isDestination = token == destinationToken;

      if ( isDestination )
      {
        resultToken = backtrackCompletedToken( token, ExecutionType.Forward );
      }
      else
      {
        NodeToken backtrackToken = backtrackToken( token );
        backtrackToken.markBacktracked( engine );
        backtrackToken.markComplete( engine );
      }
    }

    return resultToken;
  }

  private NodeToken backtrackCompletedToken (NodeToken token, ExecutionType executionType)
  {
    token.markBacktracked( engine );

    List<ArcToken> parents = new ArrayList<ArcToken>( token.getChildTokens().size() );
    for ( ArcToken childToken : token.getChildTokens() )
    {
      ArcToken parent = arcTokenMap.get( childToken );
      if ( parent == null )
      {
        throw new RuntimeException( "No backtrack found for: " + childToken );
      }
      parents.add( parent );
    }

    NodeToken backtrackToken =
      engine.getFactory().newNodeToken( token.getProcess(),
                                        token.getNode(),
                                        executionType,
                                        parents,
                                        token );
    token.getProcess().addNodeToken( backtrackToken );

    for ( ArcToken parent : parents )
    {
      token.getProcess().removeActiveArcToken( parent );
      parent.markBacktracked( engine );
      parent.markComplete( engine, backtrackToken );
    }

    return backtrackToken;
  }

  private NodeToken backtrackToken (NodeToken token)
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
      if ( token.getChildTokens().isEmpty() )
      {
        token.markBacktracked( engine );
      }
      else
      {
        backtrackToken = backtrackCompletedToken( token, ExecutionType.Backtracked );
        backtrackToken.recordGuardAction( engine, GuardAction.SkipNode );
      }
    }

    for ( ArcToken parent : getParents( token ) )
    {
      boolean backtrackParent = visited.contains( parent.getParentToken() );

      token.getProcess().removeActiveArcToken( parent );
      parent.markBacktracked( engine );

      ArcToken backtrackArcToken =
        engine.getFactory().newArcToken( token.getProcess(),
                                         parent.getArc(),
                                         backtrackParent ? ExecutionType.Backtracked : ExecutionType.UTurn,
                                         backtrackToken );

      backtrackToken.getChildTokens().add( backtrackArcToken );

      if ( backtrackParent && parent.getExecutionType() != ExecutionType.Forward )
      {
        ArcToken mirror = backtrackMirror.getMirror( parent );
        arcTokenMap.put( mirror, backtrackArcToken );
        finishArcTokenBacktrack( backtrackArcToken, mirror );
      }
      else
      {
        finishArcTokenBacktrack( backtrackArcToken, parent );
      }
    }

    return backtrackToken;
  }

  protected void finishArcTokenBacktrack (ArcToken backtrackArcToken, ArcToken parent)
  {
    arcTokenMap.put( parent, backtrackArcToken );
    NodeToken nodeTokenParent = parent.getParentToken();
    if ( visited.contains( nodeTokenParent ) )
    {
      queue.add( nodeTokenParent );
      backtrackArcToken.markProcessed( engine );
    }
    else
    {
      parent.getProcess().enqueueArcTokenForExecution( backtrackArcToken );
    }
  }

  private List<ArcToken> getParents (NodeToken token)
  {
    NodeToken related = parentMap.get( token );
    return (related == null ? token : related).getParentTokens();
  }
}