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

package org.codemonk.org.guardlang;

import org.codemonk.wf.guardlang.GuardEnv;
import org.codemonk.wf.guardlang.GuardLang;

public class TestGuardLang
{
  public static void main( String[] args )
  {
    GuardEnv env  = new GuardEnv ()
    {
      @Override
      public boolean eval( String predicate )
      {
        System.out.println( "Evaluating guard predicate: " + predicate );
        return true;
      }
    };

    System.out.println( GuardLang.eval( "IF test1 OR test2 THEN IF test3 AND NOT test4 THEN Accept ELSE Skip ELSE Skip reject", env ) );
    System.out.println( GuardLang.eval( "IF NOT test1 OR NOT test2 THEN IF test3 AND NOT test4 THEN Accept ELSE Skip ELSE Skip reject", env ) );
    System.out.println( GuardLang.eval( "IF ( test1 AND test2 ) THEN IF test3 THEN Accept ELSE Skip ELSE Skip reject", env ) );
    System.out.println( GuardLang.eval( "IF ( NOT test1 AND NOT test2 ) OR (test3 AND test4) THEN Discard ELSE Skip", env ) );
  }
}
