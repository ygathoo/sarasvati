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
package com.googlecode.sarasvati;

import java.io.File;

import org.junit.Before;

import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.mem.MemEngine;
import com.googlecode.sarasvati.mem.MemGraph;

public class ExecutionTest
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