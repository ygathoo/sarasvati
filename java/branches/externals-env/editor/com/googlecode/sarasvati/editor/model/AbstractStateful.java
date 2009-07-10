package com.googlecode.sarasvati.editor.model;

import java.util.LinkedList;

public abstract class AbstractStateful<T>
{
  private final LinkedList<T> stateStack = new LinkedList<T>();
  private final Notifier<AbstractStateful<?>> notifier = new Notifier<AbstractStateful<?>>();

  public AbstractStateful (T initialState)
  {
    pushState( initialState );
  }

  public void pushState (T memberState)
  {
    stateStack.push( memberState );
    stateChanged();
  }

  public void popState ()
  {
    stateStack.pop();
    stateChanged();
  }

  public T getState ()
  {
    return stateStack.getFirst();
  }

  public void addListener (ModelListener<? extends AbstractStateful<?>> nodeListener )
  {
    notifier.addListener( nodeListener );
  }

  protected void stateChanged ()
  {
    notifier.notify( this );
  }
}
