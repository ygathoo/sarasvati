package com.googlecode.sarasvati.perf;

import java.util.LinkedList;
import java.util.List;

import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class TestPerfStats
{
  private XmlProcessDefinition xmlProcDef;

  private List<Long> insertAvgs = new LinkedList<Long>();
  private List<Long> loadAvgs = new LinkedList<Long>();

  private long currentInsert = 0L;
  private int countInsert = 0;

  private long currentLoad = 0L;
  private int countLoad = 0;

  private static int rollover;

  public TestPerfStats (final XmlProcessDefinition xmlProcDef)
  {
    this.xmlProcDef = xmlProcDef;
  }

  public void addInsertStat (long stat)
  {
    currentInsert += stat;
    countInsert++;

    if ( countInsert == rollover )
    {
      insertAvgs.add( currentInsert / countInsert );
      currentInsert = 0;
      countInsert = 0;
    }
  }

  public void addLoadStat (long stat)
  {
    currentLoad += stat;
    countLoad++;

    if ( countLoad == rollover )
    {
      loadAvgs.add( currentLoad / countLoad );
      currentLoad = 0;
      countLoad = 0;
    }
  }

  public String getName ()
  {
    return xmlProcDef.getName();
  }

  public XmlProcessDefinition getXmlProcDef ()
  {
    return xmlProcDef;
  }

  public void setXmlProcDef (XmlProcessDefinition xmlProcDef)
  {
    this.xmlProcDef = xmlProcDef;
  }

  public static int getRollover ()
  {
    return rollover;
  }

  public static void setRollover (int rollover)
  {
    TestPerfStats.rollover = rollover;
  }

  public void dumpStats ()
  {
    System.out.println( "Graph: " + getName() );
    int count = 0;
    for ( long time : insertAvgs )
    {
      System.out.println( "Insert avg " + (++count) + ": " + time );
    }
    count = 0;
    for ( long time : loadAvgs )
    {
      System.out.println( "Load avg " + (++count) + ": " + time );
    }
  }
}