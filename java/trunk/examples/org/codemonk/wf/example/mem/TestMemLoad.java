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

package org.codemonk.wf.example.mem;

import java.io.File;

import org.codemonk.wf.ImportException;
import org.codemonk.wf.mem.MemNode;
import org.codemonk.wf.mem.MemWfEngine;
import org.codemonk.wf.mem.MemWfLoader;
import org.codemonk.wf.test.XmlTaskDef;
import org.codemonk.wf.xml.DefaultFileXmlWorkflowResolver;
import org.codemonk.wf.xml.XmlLoader;
import org.codemonk.wf.xml.XmlWorkflowResolver;

public class TestMemLoad
{
  public static void main (String[] args) throws Exception
  {
    MemWfEngine engine = new MemWfEngine();
    XmlLoader xmlLoader = new XmlLoader( XmlTaskDef.class );
    MemWfLoader wfLoader = new MemWfLoader();

    wfLoader.addCustomType( "task", new MemWfLoader.NodeFactory()
    {
      @Override
      public MemNode createNode( MemNode node, Object custom )
        throws ImportException
      {
        if ( custom == null || !(custom instanceof XmlTaskDef) )
        {
          throw new ImportException( "Task node '" + node.getName() +
                                     "' in definition of '" + node.getGraph().getName() +
                                     "' contains no (or improperly specified) task-def element." );
        }

        XmlTaskDef taskDef = (XmlTaskDef)custom;
        return node;
      }
    });

    XmlWorkflowResolver resolver = new DefaultFileXmlWorkflowResolver(xmlLoader, new File( "/home/paul/workspace/wf-common/test-wf/" ) );
    wfLoader.importWithDependencies( "embedded-task-rej", resolver );
  }
}
