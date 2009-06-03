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
}
