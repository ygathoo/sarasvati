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

package com.googlecode.sarasvati.mem;

import com.googlecode.sarasvati.impl.BaseEngine;
import com.googlecode.sarasvati.load.GraphLoader;

public class MemEngine extends BaseEngine
{
  /**
   * Creates a new MemEngine with the default application context.
   * Each application context can have it's own set of global
   * listeners.
   */
  public MemEngine ()
  {
    super( DEFAULT_APPLICATION_CONTEXT );
  }

  /**
   * Creates a new MemEngine with the given application context.
   * Each application context has it's own set of global listeners.
   *
   * This allows different applications running the same JVM to
   * have different sets of listeners without having to add
   * them at the process level.
   *
   * @param applicationContext The application context
   */
  public MemEngine (final String applicationContext)
  {
    super( applicationContext );
  }

  @Override
  public MemGraphFactory getFactory ()
  {
    return MemGraphFactory.INSTANCE;
  }

  @Override
  public MemGraphRepository getRepository ()
  {
    return MemGraphRepository.INSTANCE;
  }

  @Override
  public GraphLoader<MemGraph> getLoader()
  {
    return new GraphLoader<MemGraph>( MemGraphFactory.INSTANCE, MemGraphRepository.INSTANCE );
  }

  @Override
  public MemEngine newEngine ()
  {
    return new MemEngine ();
  }
}