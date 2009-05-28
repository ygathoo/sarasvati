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

package com.googlecode.sarasvati.example.hib;

import java.io.File;
import java.io.FilenameFilter;

import org.hibernate.Session;

import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.example.CustomTestNode;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.hib.HibGraph;
import com.googlecode.sarasvati.hib.HibNode;
import com.googlecode.sarasvati.impl.NestedProcessNode;
import com.googlecode.sarasvati.impl.ScriptNode;
import com.googlecode.sarasvati.impl.WaitNode;
import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.xml.DefaultFileXmlProcessDefinitionResolver;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinitionResolver;

public class TestHibLoad
{
  public static void main (String[] args) throws Exception
  {
    HibTestSetup.init(true);

    Session sess = HibTestSetup.openSession();
    sess.beginTransaction();

    HibEngine engine = new HibEngine( sess );
    XmlLoader xmlLoader = new XmlLoader();

    engine.addNodeType( "node", HibNode.class);
    engine.addNodeType( "task", HibExampleTaskNode.class );
    engine.addNodeType( "init", InitNode.class );
    engine.addNodeType( "dump", DumpNode.class );
    engine.addNodeType( "async", AsyncNode.class );
    engine.addNodeType( "custom", CustomNode.class );
    engine.addNodeType( "script", ScriptNode.class );
    engine.addNodeType( "nested", NestedProcessNode.class );
    engine.addNodeType( "wait", WaitNode.class );
    engine.addNodeType( "dumpTypeDupe", DumpNode.class );
    engine.addNodeType( "customTest", CustomTestNode.class );
    engine.addNodeType( "async", AsyncNode.class );

    GraphLoader<HibGraph> wfLoader = engine.getLoader();

    File baseDir = new File( "common/test-wf/" );
    assert baseDir.exists() : "Workflow process def dir not found.";

    XmlProcessDefinitionResolver resolver = new DefaultFileXmlProcessDefinitionResolver( xmlLoader, baseDir );

    FilenameFilter filter = new FilenameFilter()
    {
      @Override
      public boolean accept( File dir, String name )
      {
        return name.endsWith( ".wf.xml" ) && !name.equals( "demo-example.wf.xml" );
      }
    };

    for ( File file : baseDir.listFiles( filter ) )
    {
      String name = file.getName();
      name = name.substring( 0, name.length() - ".wf.xml".length() );

      try
      {
        wfLoader.loadWithDependencies( name, resolver );
        System.out.println( "Loaded " + name );
      }
      catch ( Exception t )
      {
        System.out.println( "Failed to load: " + name + "  because: " + t.getMessage() );
        t.printStackTrace();
        return;
      }
    }

    sess.getTransaction().commit();
  }
}
