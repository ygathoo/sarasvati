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

package com.googlecode.sarasvati.example.jdbc;

import java.io.File;
import java.io.FilenameFilter;
import java.sql.Connection;

import com.googlecode.sarasvati.jdbc.JdbcEngine;
import com.googlecode.sarasvati.jdbc.JdbcGraph;
import com.googlecode.sarasvati.jdbc.dialect.PostgreSQLDatabaseDialect;
import com.googlecode.sarasvati.load.GraphLoader;
import com.googlecode.sarasvati.xml.DefaultFileXmlProcessDefinitionResolver;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinitionResolver;

public class TestJdbcLoad
{
  public static void main (String[] args) throws Exception
  {
    Connection conn = JdbcTestSetup.openConnection();
    conn.setAutoCommit( false );

    JdbcEngine engine = new JdbcEngine( conn, new PostgreSQLDatabaseDialect() );
    XmlLoader xmlLoader = new XmlLoader();

//    engine.addNodeType( "task", TaskNode.class );
//    engine.addNodeType( "init", InitNode.class );
//    engine.addNodeType( "dump", DumpNode.class );
//    engine.addNodeType( "customTest", CustomTestNode.class );

    GraphLoader<JdbcGraph> wfLoader = engine.getLoader();

    File baseDir = new File( "/home/paul/workspace/wf-common/test-wf/" );

    XmlProcessDefinitionResolver resolver = new DefaultFileXmlProcessDefinitionResolver( xmlLoader, baseDir );

    FilenameFilter filter = new FilenameFilter()
    {
      @Override
      public boolean accept( File dir, String name )
      {
        return name.endsWith( ".wf.xml" );
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

    conn.commit();
  }
}
