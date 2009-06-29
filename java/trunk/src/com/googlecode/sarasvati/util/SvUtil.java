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

package com.googlecode.sarasvati.util;

import com.googlecode.sarasvati.Token;
import com.googlecode.sarasvati.TokenSet;
import com.googlecode.sarasvati.TokenSetMember;

public class SvUtil
{
  public static boolean equals (Object o1, Object o2)
  {
    if ( o1 == null )
    {
      return o2 == null;
    }
    return o1.equals( o2 );
  }

  public static boolean isBlankOrNull (String str)
  {
    return str == null || str.trim().equals( "" );
  }

  public static String nullIfBlank (String str)
  {
    return isBlankOrNull( str ) ? null : str;
  }

  public static String normalizeQuotedString (String string)
  {
    if ( string == null )
    {
      return null;
    }

    StringBuilder buf = new StringBuilder( string.substring( 1, string.length() - 1 ) );

    for ( int i = 0; i < buf.length(); i++ )
    {
      if ( i != buf.length() - 1 &&
          buf.charAt( i ) == '\\' &&
          buf.charAt( i + 1 ) == '"' )
      {
        buf.deleteCharAt( i );
        i--;
      }
    }

    return buf.toString();
  }

  public static TokenSet getTokenSet (Token token, String name)
  {
    for ( TokenSetMember setMember : token.getTokenSetMemberships() )
    {
      if ( SvUtil.equals( name, setMember.getTokenSet().getName() ) )
      {
        return setMember.getTokenSet();
      }
    }
    return null;
  }

  public static TokenSetMember getTokenSetMember (Token token, String name)
  {
    for ( TokenSetMember setMember : token.getTokenSetMemberships() )
    {
      if ( SvUtil.equals( name, setMember.getTokenSet().getName() ) )
      {
        return setMember;
      }
    }
    return null;
  }
}
