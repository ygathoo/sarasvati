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

import java.util.Arrays;
import java.util.List;

import org.hibernate.Query;
import org.hibernate.Session;
import org.hibernate.cfg.AnnotationConfiguration;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.event.ExecutionEventType;
import com.googlecode.sarasvati.event.ExecutionListener;
import com.googlecode.sarasvati.impl.BaseEngine;
import com.googlecode.sarasvati.load.GraphLoader;

public class HibEngine extends BaseEngine
{
  protected Session session;
  protected HibGraphFactory factory;
  protected HibGraphRepository repository;

  /**
   * This constructor can be used in cases when the session will be set in later
   * or when performing operations that don't require a session, such as adding
   * global execution listeners.
   */
  public HibEngine ()
  {
    // Default constructor
  }

  public HibEngine (final Session session)
  {
    this.session = session;
    this.factory = new HibGraphFactory( session );
    this.repository = new HibGraphRepository( session );
  }

  public Session getSession ()
  {
    return session;
  }

  public void setSession (final Session session)
  {
    this.session = session;
    this.factory = new HibGraphFactory( session );
    this.repository = new HibGraphRepository( session );
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
  public GraphLoader<HibGraph> getLoader()
  {
    return new GraphLoader<HibGraph>( factory, repository );
  }

  @Override
  public void addExecutionListener(final GraphProcess process,
                                   final Class<? extends ExecutionListener> listenerClass,
                                   final ExecutionEventType... eventTypes)
  {
    if ( eventTypes == null || listenerClass == null )
    {
      return;
    }

    for ( ExecutionEventType eventType : eventTypes )
    {
      if ( eventType != null )
      {
        HibProcessListener hibListener = new HibProcessListener( listenerClass.getName(), eventType, process );
        session.save( hibListener );
      }
    }

    super.addExecutionListener( process, listenerClass, eventTypes );
  }

  @Override
  public void removeExecutionListener(final GraphProcess process,
                                      final Class<? extends ExecutionListener> listenerClass,
                                      final ExecutionEventType... eventTypes)
  {
    List<ExecutionEventType> types = eventTypes == null ? null :  Arrays.asList( eventTypes );

    for ( HibProcessListener hibListener : ((HibGraphProcess)process).getListeners() )
    {
      if ( process.equals( hibListener.getProcess() ) &&
           (eventTypes == null || eventTypes.length == 0 || types.contains( hibListener.getEventType() ) ) )
      {
        session.delete( hibListener );
      }
    }

    super.removeExecutionListener( process, listenerClass, eventTypes );
  }

  @Override
  public HibEngine newEngine ()
  {
    HibEngine engine = new HibEngine();

    engine.session = session;
    engine.factory = factory;
    engine.repository = repository;

    return engine;
  }

  @SuppressWarnings("unchecked")
  public List<ArcToken> getActiveArcTokens (final HibTokenSet tokenSet)
  {
    String hql = "select token from HibArcToken token inner join token.tokenSetMemberships as setMember " +
                 "where token.completeDate is null and setMember.tokenSet = :tokenSet";
    Query query = session.createQuery( hql ).setEntity( "tokenSet", tokenSet );
    return query.list();
  }

  @SuppressWarnings("unchecked")
  public List<NodeToken> getActiveNodeTokens (final HibTokenSet tokenSet)
  {
    String hql = "select token from HibNodeToken token inner join token.tokenSetMemberships as setMember " +
                 "where token.completeDate is null and setMember.tokenSet = :tokenSet";
    Query query = session.createQuery( hql ).setEntity( "tokenSet", tokenSet );
    return query.list();
  }

  public static void addToConfiguration (final AnnotationConfiguration config, final boolean enableCaching)
  {
    config.addAnnotatedClass( HibArc.class );
    config.addAnnotatedClass( HibArcToken.class );
    config.addAnnotatedClass( HibGraph.class );
    config.addAnnotatedClass( HibProcessListener.class );
    config.addAnnotatedClass( HibNode.class );
    config.addAnnotatedClass( HibNodeRef.class );
    config.addAnnotatedClass( HibNodeToken.class );
    config.addAnnotatedClass( HibGraphProcess.class );
    config.addAnnotatedClass( HibPropertyNode.class );
    config.addAnnotatedClass( HibCustomNodeWrapper.class );
    config.addAnnotatedClass( HibTokenSet.class );
    config.addAnnotatedClass( HibArcTokenSetMember.class );
    config.addAnnotatedClass( HibNodeTokenSetMember.class );
    config.addAnnotatedClass( HibNodeType.class );
    config.addAnnotatedClass( HibTokenSetMemberAttribute.class );
    config.addAnnotatedClass( HibExternal.class );

    if ( enableCaching )
    {
      config.setCacheConcurrencyStrategy( HibGraph.class.getName(),"read-write" );
      config.setCollectionCacheConcurrencyStrategy( HibGraph.class.getName() + ".nodes", "read-write" );
      config.setCollectionCacheConcurrencyStrategy( HibGraph.class.getName() + ".arcs", "read-write" );

      config.setCacheConcurrencyStrategy( HibNode.class.getName(),"read-write" );
      config.setCacheConcurrencyStrategy( HibNodeRef.class.getName(),"read-write" );
      config.setCacheConcurrencyStrategy( HibArc.class.getName(),"read-write" );
      config.setCacheConcurrencyStrategy( HibExternal.class.getName(),"read-write" );

      config.setCacheConcurrencyStrategy( HibGraphProcess.class.getName(),"read-write" );
      config.setCollectionCacheConcurrencyStrategy( HibGraphProcess.class.getName() + ".listeners", "read-write" );
      config.setCollectionCacheConcurrencyStrategy( HibGraphProcess.class.getName() + ".activeArcTokens", "read-write" );
      config.setCollectionCacheConcurrencyStrategy( HibGraphProcess.class.getName() + ".activeNodeTokens", "read-write" );
      config.setCollectionCacheConcurrencyStrategy( HibGraphProcess.class.getName() + ".executionQueue", "read-write" );

      config.setCacheConcurrencyStrategy( HibProcessListener.class.getName(),"read-write" );
      config.setCacheConcurrencyStrategy( HibNodeToken.class.getName(),"read-write" );
      config.setCacheConcurrencyStrategy( HibArcToken.class.getName(),"read-write" );
    }
  }
}