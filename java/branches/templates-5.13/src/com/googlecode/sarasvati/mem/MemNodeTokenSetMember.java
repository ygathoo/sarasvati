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

    Copyright 2009 Paul Lorenz
*/

package com.googlecode.sarasvati.mem;

import com.googlecode.sarasvati.NodeTokenSetMember;
import com.googlecode.sarasvati.TokenSet;

public class MemNodeTokenSetMember implements NodeTokenSetMember
{
  protected TokenSet tokenSet;
  protected MemNodeToken token;
  protected int memberIndex;

  public MemNodeTokenSetMember (TokenSet tokenSet, MemNodeToken token, int memberIndex)
  {
    this.tokenSet = tokenSet;
    this.token = token;
    this.memberIndex = memberIndex;
  }

  @Override
  public TokenSet getTokenSet ()
  {
    return tokenSet;
  }

  @Override
  public MemNodeToken getToken ()
  {
    return token;
  }

  @Override
  public int getMemberIndex ()
  {
    return memberIndex;
  }
}