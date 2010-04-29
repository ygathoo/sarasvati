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
package com.googlecode.sarasvati.event;

import java.util.concurrent.ConcurrentHashMap;

import com.googlecode.sarasvati.SarasvatiException;

public class ListenerCache
{
  protected ConcurrentHashMap<String, ExecutionListener> listenerCache = new ConcurrentHashMap<String, ExecutionListener>();

  @SuppressWarnings("unchecked")
  public ExecutionListener getListener (final String type)
  {
    ExecutionListener listener = listenerCache.get( type );

    if ( listener == null )
    {
      Class<? extends ExecutionListener> listenerClass = null;

      try
      {
        listenerClass = (Class< ? extends ExecutionListener>)Class.forName( type );
      }
      catch (Exception e)
      {
        throw new SarasvatiException( "Failed to load ExecutionListener class: " + type, e );
      }

      try
      {
        listener = listenerClass.newInstance();
      }
      catch ( InstantiationException e )
      {
        throw new SarasvatiException( "ExecutionListeners must have a default public constructor. " +
                                      "They may not be non-static inner classes. " +
                                      "In other words, you must be able create new ones using listenerClass.newInstance()",
                                      e );
      }
      catch ( IllegalAccessException e )
      {
        throw new SarasvatiException( "ExecutionListeners must have a default public constructor. " +
                                      "They may not be non-static inner classes. " +
                                      "In other words, you must be able create new ones using listenerClass.newInstance()",
                                      e );
      }
    }

    return listener;
  }

  public ExecutionListener getListener (final Class<? extends ExecutionListener> listenerClass)
  {
    ExecutionListener listener = listenerCache.get( listenerClass.getName() );

    if ( listener == null )
    {
      try
      {
        listener = listenerClass.newInstance();
      }
      catch ( InstantiationException e )
      {
        throw new SarasvatiException( "ExecutionListeners must have a default public constructor. " +
                                      "They may not be non-static inner classes. " +
                                      "In other words, you must be able create new ones using listenerClass.newInstance()",
                                      e );
      }
      catch ( IllegalAccessException e )
      {
        throw new SarasvatiException( "ExecutionListeners must have a default public constructor. " +
                                      "They may not be non-static inner classes. " +
                                      "In other words, you must be able create new ones using listenerClass.newInstance()",
                                      e );
      }
    }

    return listener;
  }
}