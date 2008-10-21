package com.googlecode.sarasvati.visual;

import javax.swing.SwingUtilities;

public class VisualUtil
{
  protected static final Runnable EmptyRunnable = new Runnable()
  {
    public void run()
    {
      // does nothing
    }
  };

  public static void flushAWTQueue ()
  {
    if ( SwingUtilities.isEventDispatchThread() )
    {
      return;
    }

    try
    {
      SwingUtilities.invokeAndWait( EmptyRunnable );
    }
    catch (Exception e)
    {
      // ignore
    }
  }
}
