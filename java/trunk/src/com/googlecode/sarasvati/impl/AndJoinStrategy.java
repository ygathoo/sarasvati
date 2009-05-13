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
package com.googlecode.sarasvati.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.JoinResult;
import com.googlecode.sarasvati.JoinStrategy;

public class AndJoinStrategy implements JoinStrategy
{
  protected List<? extends Arc> getJoiningArcs (GraphProcess process, ArcToken token)
  {
    return process.getGraph().getInputArcs( token.getArc().getEndNode() );
  }
  
  @Override
  public JoinResult performJoin (GraphProcess process, ArcToken token)
  {
    List<? extends Arc> inputs = getJoiningArcs( process, token );

    ArrayList<ArcToken> tokens = new ArrayList<ArcToken>( inputs.size() );

    for ( Arc arc : inputs )
    {
      for ( ArcToken arcToken : process.getActiveArcTokens() )
      {
        if ( arcToken.getArc().equals( arc ) )
        {
          tokens.add( arcToken );
          break;
        }
      }
    }

    return inputs.size() == tokens.size() ?
        new CompleteJoinResult( Collections.singletonList( token ) ) :
        JoinResult.INCOMPLETE_JOIN_RESULT;
  }
}
