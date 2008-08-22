package com.googlecode.sarasvati.util;

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
}
