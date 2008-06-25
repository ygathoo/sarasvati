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

package com.googlecode.sarasvati.example.db;

import java.io.File;
import java.io.FilenameFilter;

import org.hibernate.Session;

import com.googlecode.sarasvati.ImportException;
import com.googlecode.sarasvati.example.XmlTaskDef;
import com.googlecode.sarasvati.hib.HibNode;
import com.googlecode.sarasvati.hib.HibWfEngine;
import com.googlecode.sarasvati.hib.HibWfLoader;
import com.googlecode.sarasvati.xml.DefaultFileXmlWorkflowResolver;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlWorkflowResolver;

public class TestDbLoad
{
  public static void main (String[] args) throws Exception
  {
    TestSetup.init();

    Session sess = TestSetup.openSession();
    sess.beginTransaction();

    HibWfEngine engine = new HibWfEngine( sess );
    XmlLoader xmlLoader = new XmlLoader( XmlTaskDef.class );
    HibWfLoader wfLoader = new HibWfLoader( engine );

    wfLoader.addCustomType( "task", new HibWfLoader.NodeFactory()
    {
      @Override
      public HibNode createNode( HibWfEngine wfEngine, HibNode node, Object custom )
        throws ImportException
      {
        if ( custom == null || !(custom instanceof XmlTaskDef) )
        {
          throw new ImportException( "Task node '" + node.getName() +
                                     "' in definition of '" + node.getGraph().getName() +
                                     "' contains no (or improperly specified) task-def element." );
        }

        XmlTaskDef taskDef = (XmlTaskDef)custom;

        NodeTask nodeTask = new NodeTask( node );
        wfEngine.getSession().save( nodeTask );

        nodeTask.getDetail().setId( nodeTask.getId() );
        nodeTask.getDetail().setTaskName( taskDef.getTaskName() );
        nodeTask.getDetail().setTaskDesc( taskDef.getDescription().trim() );
        wfEngine.getSession().save( nodeTask.getDetail() );

        return nodeTask;
      }
    });

    File baseDir = new File( "/home/paul/workspace/wf-common/test-wf/" );

    XmlWorkflowResolver resolver = new DefaultFileXmlWorkflowResolver(xmlLoader, baseDir );

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
        wfLoader.importWithDependencies( name, resolver );
      }
      catch ( Exception t )
      {
        System.out.println( "Failed to load: " + name + "  because: " + t.getMessage() );
        t.printStackTrace();
      }
    }

    sess.getTransaction().commit();
  }
}
