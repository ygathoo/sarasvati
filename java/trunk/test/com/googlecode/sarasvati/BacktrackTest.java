package com.googlecode.sarasvati;

import java.io.File;

import org.junit.Before;

import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.mem.MemEngine;
import com.googlecode.sarasvati.mem.MemGraph;

public class BacktrackTest
{
  protected MemEngine engine;

  @Before
  public void setup ()
  {
    engine = new MemEngine();
  }

  protected Graph ensureLoaded (String name) throws Exception
  {
    File basePath = new File( "/home/paul/workspace/wf-common/unit-test/" );
    GraphLoader<MemGraph> loader = engine.getLoader();

    if ( !loader.isLoaded( name ) )
    {
      loader.load( new File( basePath, name + ".wf.xml" ) );
    }
    return engine.getRepository().getLatestGraph( name );
  }
}