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

package com.googlecode.sarasvati.guardlang;

import java.io.IOException;
import java.util.StringTokenizer;

public class GuardLangLexer implements GuardLangParser.yyInput
{
  public String input;
  private String value;
  private int token;
  private StringTokenizer tok;

  public GuardLangLexer (String input)
  {
    this.input = input;
    this.value = null;
    this.tok = new StringTokenizer( input, " \t\r\n()", true );
  }

  @Override
  public boolean advance() throws IOException
  {
    if ( !tok.hasMoreTokens() )
    {
      return false;
    }

    value = null;

    String next = tok.nextToken();
    if ( "(".equals( next ) )
    {
      token = '(';
    }
    else if ( ")".equals( next ) )
    {
      token = ')';
    }
    else if ( "IF".equals( next ) )
    {
      token = GuardLangParser.IF;
    }
    else if ( "THEN".equals( next ) )
    {
      token = GuardLangParser.THEN;
    }
    else if ( "ELSE".equals( next ) )
    {
      token = GuardLangParser.ELSE;
    }
    else if ( "AND".equals( next ) )
    {
      token = GuardLangParser.AND;
    }
    else if ( "OR".equals( next ) )
    {
      token = GuardLangParser.OR;
    }
    else if ( "NOT".equals( next ) )
    {
      token = GuardLangParser.NOT;
    }
    else if ( "Accept".equals( next ) )
    {
      token = GuardLangParser.ACCEPT;
    }
    else if ( "Discard".equals( next ) )
    {
      token = GuardLangParser.DISCARD;
    }
    else if ( "Skip".equals( next ) )
    {
      token = GuardLangParser.SKIP;
    }
    else if ( isSymbol( next ) )
    {
      token = GuardLangParser.SYMBOL;
      value = next;
    }
    else if ( isWhitespace( next ) )
    {
      return advance();
    }
    else
    {
      throw new IOException( "Invalid token " + next );
    }

    return true;
  }

  public static boolean isSymbol ( String next )
  {
    if ( !Character.isLetter(  next.charAt( 0 ) ) )
    {
      return false;
    }

    for ( int i = 1; i < next.length(); i++ )
    {
      char c = next.charAt( i );
      if ( !Character.isLetterOrDigit( c ) && '.' != c && '_' != c )
      {
        return false;
      }
    }

    return true;
  }

  public static boolean isWhitespace (String next)
  {
    for ( int i = 0; i < next.length(); i++ )
    {
      if ( !Character.isWhitespace( next.charAt( i ) ) )
      {
        return false;
      }
    }

    return true;
  }

  @Override
  public int token()
  {
    return token;
  }

  @Override
  public Object value()
  {
    return value;
  }
}
