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

package com.googlecode.sarasvati.hib;

import java.util.List;

import org.hibernate.Session;
import org.hibernate.cfg.AnnotationConfiguration;

import com.googlecode.sarasvati.NonRecursiveEngine;
import com.googlecode.sarasvati.Process;
import com.googlecode.sarasvati.WorkflowException;
import com.googlecode.sarasvati.event.ExecutionEvent;
import com.googlecode.sarasvati.event.DefaultExecutionEventQueue;
import com.googlecode.sarasvati.event.ExecutionEventQueue;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.event.ExecutionListener;
import com.googlecode.sarasvati.event.ListenerCache;

public class HibEngine extends NonRecursiveEngine
{
  protected static final ExecutionEventQueue globalEventDispatcher = DefaultExecutionEventQueue.newCopyOnWriteListInstance();
  protected static final ListenerCache            listenerCache          = new ListenerCache();

  protected Session session;
  protected HibGraphFactory factory;
  protected HibGraphRepository repository;

  public HibEngine (Session session)
  {
    this.session = session;
    this.factory = new HibGraphFactory( session );
    this.repository = new HibGraphRepository( session );
  }

  public Session getSession ()
  {
    return session;
  }

  public void setSession (Session session)
  {
    this.session = session;
  }

  @Override
  public HibGraphRepository getRepository()
  {
    return repository;
  }

  @Override
  public HibGraphFactory getFactory()
  {
    return factory;
  }

  @Override
  public void fireEvent(ExecutionEvent event)
  {
    globalEventDispatcher.fireEvent( event );
    event.getProcess().getEventQueue().fireEvent( event );
  }

  @Override
  public void addExecutionListener(Process process, ExecutionListener listener, ExecutionEventType... eventTypes)
  {
    if ( eventTypes == null || listener == null )
    {
      return;
    }

    ExecutionEventQueue eventDispatcher = process == null ? globalEventDispatcher : process.getEventQueue();

    for ( ExecutionEventType eventType : eventTypes )
    {
      if ( eventType == null )
      {
        continue;
      }

      HibProcessListener hibListener = new HibProcessListener( listener.getClass().getName(), eventType, process );
      session.save( hibListener );
      eventDispatcher.addListener( this, listener, eventTypes );

      listenerCache.ensureContainsListenerType( listener );
    }
  }

  @SuppressWarnings("unchecked")
  public void initGlobalListeners ()
  {
    List<HibProcessListener> hibListeners = session.createQuery( "from HibProcessListener where process is null" ).list();
    initFromDatabase( hibListeners, globalEventDispatcher );
  }

  private void initFromDatabase (List<HibProcessListener> hibListeners, ExecutionEventQueue eventDispatcher)
  {
    for ( HibProcessListener hibListener : hibListeners )
    {
      ExecutionListener listener = getExecutionListenerInstance( hibListener.getType() );
      eventDispatcher.addListener( this, listener, hibListener.getEventType() );
    }
  }

  @Override
  public ExecutionListener getExecutionListenerInstance (String type) throws WorkflowException
  {
    return listenerCache.getListener( type );
  }

  public static void addToConfiguration (AnnotationConfiguration config, boolean enableCaching)
  {
    config.addAnnotatedClass( HibArc.class );
    config.addAnnotatedClass( HibArcToken.class );
    config.addAnnotatedClass( HibGraph.class );
    config.addAnnotatedClass( HibNode.class );
    config.addAnnotatedClass( HibNodeRef.class );
    config.addAnnotatedClass( HibNodeToken.class );
    config.addAnnotatedClass( HibProcess.class );

    if (enableCaching )
    {
      config.setCacheConcurrencyStrategy( HibGraph.class.getName(),"read-only" );
      config.setCacheConcurrencyStrategy( HibNode.class.getName(),"read-only" );
      config.setCacheConcurrencyStrategy( HibNodeRef.class.getName(),"read-only" );
      config.setCacheConcurrencyStrategy( HibArc.class.getName(),"read-only" );
    }
  }
}