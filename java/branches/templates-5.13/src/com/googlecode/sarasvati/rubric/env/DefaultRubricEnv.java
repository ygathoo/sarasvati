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

import java.util.Calendar;
import java.util.Date;

import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.NodeToken;

/**
 * Basic RubricEnv implementation which uses a {@link RubricFunctionRepository}
 * as a source for predicate and date function and which uses Calendar to do
 * relative date evaluations. Business days/hours are not handled differently
 * then regular relative dates.
 *
 * @author Paul Lorenz
 */
public class DefaultRubricEnv implements RubricEnv
{
  protected Engine                   engine;
  protected NodeToken                token;
  protected RubricFunctionRepository functionRepository;

  public DefaultRubricEnv (Engine engine, NodeToken token, RubricFunctionRepository functionRepository)
  {
    this.engine = engine;
    this.token = token;
    this.functionRepository = functionRepository;
  }

  @Override
  public Date evalDateFunction (String dateFunction)
  {
    return functionRepository.getDateFunction( dateFunction ).eval( engine, token );
  }

  @Override
  public boolean evalPredicate (String predicate)
  {
    return functionRepository.getPredicate( predicate ).eval( engine, token );
  }

  @Override
  public Date evalRelativeDate (Date date, boolean business, int offset, int unit)
  {
    Calendar cal = Calendar.getInstance();
    cal.setTime( date );
    cal.add( unit, offset );
    return cal.getTime();
  }
}