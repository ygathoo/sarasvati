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

package com.googlecode.sarasvati.rubric.env;

import java.util.HashMap;
import java.util.Map;

public class DefaultRubricFunctionRepository implements RubricFunctionRepository
{
  private static final DefaultRubricFunctionRepository GLOBAL_INSTANCE = new DefaultRubricFunctionRepository();

  public static DefaultRubricFunctionRepository getGlobalInstance ()
  {
    return GLOBAL_INSTANCE;
  }

  protected Map<String, RubricPredicate> predicateMap = new HashMap<String, RubricPredicate>();
  protected Map<String, RubricDateFunction> dateFunctionMap = new HashMap<String, RubricDateFunction>();

  public void registerPredicate (String name, RubricPredicate predicate)
  {
    predicateMap.put( name, predicate );
  }

  public void unregisterPredicate (String name)
  {
    predicateMap.remove( name );
  }

  public void registerDateFunction (String name, RubricDateFunction dateFunction)
  {
    dateFunctionMap.put( name, dateFunction );
  }

  public void unregisterDateFunction (String name)
  {
    dateFunctionMap.remove( name );
  }

  /**
   * @see com.googlecode.sarasvati.rubric.env.RubricFunctionRepository#getPredicate(java.lang.String)
   */
  public RubricPredicate getPredicate (String predicate)
  {
    return predicateMap.get( predicate );
  }

  /**
   * @see com.googlecode.sarasvati.rubric.env.RubricFunctionRepository#getDateFunction(java.lang.String)
   */
  public RubricDateFunction getDateFunction (String dateFunction)
  {
    return dateFunctionMap.get( dateFunction );
  }
}