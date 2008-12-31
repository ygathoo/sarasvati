package com.googlecode.sarasvati.visitor;

import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.ExecutionType;

public class BacktrackMirrors
{
  protected Map<ArcToken, ArcToken> map = new HashMap<ArcToken, ArcToken> ();

  public void add (ArcToken token)
  {
    map.put( token, null );
  }

  public boolean hasMirror (ArcToken token)
  {
    return getMirror( token ) != null;
  }

  public ArcToken getMirror (ArcToken token)
  {
    ArcToken result = map.get( token );
    if ( result == null )
    {
      findBacktrackMirror( token );
      result = map.get( token );
    }

    return result;
  }

  public void findBacktrackMirror (ArcToken token)
  {
    for ( ArcToken parent : token.getParentToken().getParentTokens() )
    {
      if ( isMirror( token, parent ) )
      {
        map.put( token, parent );
        return;
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
              map.put( token, mirrorParent );
              return;
            }
          }
        }
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
}