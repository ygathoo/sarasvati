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
package org.codemonk.wf;

public interface GuardResponse
{
  static GuardResponse ACCEPT_TOKEN_RESPONSE = new GuardResponse()
  {
    @Override
    public String getExitArcForSkip()
    {
      throw new UnsupportedOperationException(
        "getExitArcsForSkip should never be called on a GuardResponse with action of AcceptToken" );
    }

    @Override
    public GuardAction getGuardAction()
    {
      return GuardAction.AcceptToken;
    }

    @Override
    public String toString()
    {
      return "AcceptTokenResponse";
    }
  };

  static GuardResponse DISCARD_TOKEN_RESPONSE = new GuardResponse()
  {
    @Override
    public String getExitArcForSkip()
    {
      throw new UnsupportedOperationException(
        "getExitArcsForSkip should never be called on a GuardResponse with action of DiscardToken" );

    }

    @Override
    public GuardAction getGuardAction()
    {
      return GuardAction.DiscardToken;
    }

    @Override
    public String toString()
    {
      return "DiscardTokenResponse";
    }
  };

  GuardAction getGuardAction ();

  String getExitArcForSkip ();
}
