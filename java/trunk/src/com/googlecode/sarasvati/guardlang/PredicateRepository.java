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

import java.util.HashMap;
import java.util.Map;

import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.Engine;

public class PredicateRepository
{
  protected static Map<String, GuardLangPredicate> predicateMap = new HashMap<String, GuardLangPredicate>();

  public static void addPredicate (String name, GuardLangPredicate predicate)
  {
    predicateMap.put( name, predicate );
  }

  public static GuardLangPredicate getPredicate (String name)
  {
    return predicateMap.get( name );
  }

  public static GuardEnv newGuardEnv (final Engine engine, final NodeToken token)
  {
    return new GuardEnv()
    {
      @Override
      public boolean eval( String name )
      {
        GuardLangPredicate predicate = PredicateRepository.getPredicate( name );
        return predicate.evaluate( engine, token );
      }
    };
  }
}
