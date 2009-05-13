package com.googlecode.sarasvati.util;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.NodeToken;

public class ProcessPrinter
{
  public static void print (GraphProcess p)
  {
    System.out.println( "Process: " + p.getGraph().getName() );
    for (NodeToken t : p.getNodeTokens() )
    {
      System.out.println( t );
      for ( ArcToken a : t.getChildTokens() )
      {
        System.out.println( "\t" + a );
      }
    }
    System.out.println();
  }
}
