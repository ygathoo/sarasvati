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

import java.io.File;
import java.io.UnsupportedEncodingException;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import com.googlecode.sarasvati.Token;
import com.googlecode.sarasvati.TokenSet;
import com.googlecode.sarasvati.TokenSetMember;
import com.googlecode.sarasvati.load.definition.ExternalDefinition;
import com.googlecode.sarasvati.load.definition.ProcessDefinition;

public class SvUtil
{
  public static boolean equals (final Object o1, final Object o2)
  {
    if ( o1 == null )
    {
      return o2 == null;
    }
    return o1.equals( o2 );
  }

  public static boolean isBlankOrNull (final String str)
  {
    return str == null || str.trim().equals( "" );
  }

  public static String nullIfBlank (final String str)
  {
    return isBlankOrNull( str ) ? null : str;
  }

  public static String blankIfNull (final String str)
  {
    return isBlankOrNull( str ) ? "" : str;
  }

  public static boolean falseIfNull (final Boolean value)
  {
    return value != null && value;
  }

  public static String normalizeQuotedString (final String string)
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

  public static TokenSet getTokenSet (final Token token, final String name)
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

  public static TokenSetMember getTokenSetMember (final Token token, final String name)
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

  public static int compare (final String s1, final String s2)
  {
    if ( s1 == null )
    {
      return s2 == null ? 0 : -1;
    }
    return s2 == null ? 1 : s1.compareTo( s2 );
  }

  public static void visitRecursive (final File basePath,
                                     final FileVisitor visitor,
                                     final boolean recurse)
  {
    Queue<File> dirs = new LinkedList<File>();

    if ( basePath.isDirectory() )
    {
      dirs.add( basePath );
    }

    while ( !dirs.isEmpty() )
    {
      File dir = dirs.remove();

      for ( File file : dir.listFiles() )
      {
        if ( recurse && file.isDirectory() )
        {
          dirs.add( file );
        }
        else if ( visitor.accept( dir, file.getName() ) )
        {
          visitor.accept( file );
        }
      }
    }
  }

  public static Collection<ProcessDefinition> getSorted (final Map<String, ProcessDefinition> processDefsByName)
  {
    Set<ProcessDefinition> processed = new LinkedHashSet<ProcessDefinition>( processDefsByName.size() );

    for ( ProcessDefinition def : processDefsByName.values() )
    {
      if ( !processed.contains( def ) )
      {
        addWithPrerequisites( def, processed, processDefsByName );
      }
    }

    return processed;
  }

  private static void addWithPrerequisites (final ProcessDefinition def,
                                            final Set<ProcessDefinition> processed,
                                            final Map<String, ProcessDefinition> processDefsByName)
  {
    for ( ExternalDefinition external : def.getExternals() )
    {
      ProcessDefinition externalPD = processDefsByName.get( external.getProcessDefinition() );
      if ( !processed.contains( externalPD ) )
      {
        addWithPrerequisites( externalPD, processed, processDefsByName );
      }
    }

    processed.add( def );
  }

  /**
   * From http://www.rgagnon.com/javadetails/java-0596.html
   */
  private static final byte[] HEX_CHAR_TABLE = {
    (byte)'0', (byte)'1', (byte)'2', (byte)'3',
    (byte)'4', (byte)'5', (byte)'6', (byte)'7',
    (byte)'8', (byte)'9', (byte)'a', (byte)'b',
    (byte)'c', (byte)'d', (byte)'e', (byte)'f'
  };

  /**
   * From http://www.rgagnon.com/javadetails/java-0596.html
   */
  public static String getHexString (final byte[] raw)
  {
    byte[] hex = new byte[2 * raw.length];
    int index = 0;

    for (byte b : raw)
    {
      int v = b & 0xFF;
      hex[index++] = HEX_CHAR_TABLE[v >>> 4];
      hex[index++] = HEX_CHAR_TABLE[v & 0xF];
    }

    try
    {
      return new String(hex, "ASCII");
    }
    catch ( UnsupportedEncodingException uee )
    {
      throw new RuntimeException( uee );
    }
  }
}