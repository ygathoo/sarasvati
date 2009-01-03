package com.googlecode.sarasvati.visitor;

import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.ExecutionType;

public class BacktrackMirrors
{
  protected Map<ArcToken, ArcToken> map          = new HashMap<ArcToken, ArcToken> ();

  public void addVisited (ArcToken token)
  {
    System.out.println( "Visited: " + token.hashCode() );
    if ( !map.containsKey( token ) )
    {
      map.put( token, null );
    }
  }

  public boolean hasMirror (ArcToken token)
  {
    return map.containsKey( token );
  }

  public void findMirror (ArcToken token)
  {
    if ( token.getExecutionType() == ExecutionType.ForwardBacktracked )
    {
      findForwardBacktrackedMirror( token );
    }
    else
    {
      findBacktrackMirror( token );
    }
  }

  private void findBacktrackMirror (ArcToken token)
  {
    for ( ArcToken parent : token.getParentToken().getParentTokens() )
    {
      if ( isMirror( token, parent ) )
      {
        System.out.println( token + " has mirror " + parent );
        map.put( token, parent );
        tieToEndOfBacktrackChain( token );
        return;
      }
      else
      {
        System.out.println( token + " NOT PARENT " + parent );
      }

      if ( parent.getExecutionType() == ExecutionType.Backtracked )
      {
        ArcToken mirror = map.get( parent );
        if ( mirror != null )
        {
          for ( ArcToken mirrorParent : mirror.getParentToken().getParentTokens() )
          {
            if ( isMirror( token, mirrorParent ) )
            {
              System.out.println( token + " has mirror " + mirrorParent );
              map.put( token, mirrorParent );
              tieToEndOfBacktrackChain( token );
              return;
            }
          }
        }
      }
    }
    System.out.println( "No mirror found for " + token );
  }

  private void findForwardBacktrackedMirror (ArcToken token)
  {
    if ( token.getParentToken().getExecutionType() != ExecutionType.ForwardBacktracked )
    {
      return;
    }

    for ( ArcToken parent : token.getParentToken().getParentTokens() )
    {
      if ( parent.getExecutionType() == ExecutionType.Backtracked &&
           token.getArc().equals( parent.getArc() ) )
      {
        System.out.println( token + " has mirror " + parent );
        map.put( token, parent );
        tieToEndOfBacktrackChain( token );
        return;
      }
    }
  }

  private boolean isMirror (ArcToken token, ArcToken candidate)
  {
    return ( candidate.getExecutionType() == ExecutionType.ForwardBacktracked ||
             candidate.getExecutionType() == ExecutionType.UTurnBacktracked ) &&
           token.getArc().equals( candidate.getArc() ) &&
           map.containsKey( candidate );
  }

  private void tieToEndOfBacktrackChain (ArcToken token)
  {
    ArcToken lookup = map.get( token );
    ArcToken result = lookup;
    while ( lookup != null )
    {
      lookup = map.get( lookup );
      if ( lookup != null )
      {
        result = lookup;
      }
    }

    map.put( token, result );
  }

  public ArcToken getMirror (ArcToken token)
  {
    ArcToken result = map.get( token );
    return result == null ? token : result;
  }
}