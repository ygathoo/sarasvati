/**
 * Created on Jul 17, 2009
 */
package com.googlecode.sarasvati.editor.command;

public abstract class AbstractCommand implements Command
{
  @Override
  public int getUndoOrder ()
  {
    return 0;
  }

  @Override
  public int compareTo (Command o)
  {
    return getUndoOrder() - o.getUndoOrder();
  }
}
