/**
 * Created on Jun 9, 2008
 */
package org.codemonk.wf.test;

import java.io.File;

import org.codemonk.wf.ImportException;
import org.codemonk.wf.hib.HibNode;
import org.codemonk.wf.hib.HibWfEngine;
import org.codemonk.wf.hib.HibWfLoader;
import org.codemonk.wf.xml.DefaultFileXmlWorkflowResolver;
import org.codemonk.wf.xml.XmlLoader;
import org.codemonk.wf.xml.XmlWorkflowResolver;
import org.hibernate.Session;

public class TestLoad
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
      public HibNode createNode( HibWfEngine engine, HibNode node, Object custom )
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
        engine.getSession().save( nodeTask );

        nodeTask.getDetail().setId( nodeTask.getId() );
        nodeTask.getDetail().setTaskName( taskDef.getTaskName() );
        nodeTask.getDetail().setTaskDesc( taskDef.getDescription().trim() );
        engine.getSession().save( nodeTask.getDetail() );

        return nodeTask;
      }
    });

    XmlWorkflowResolver resolver = new DefaultFileXmlWorkflowResolver(xmlLoader, new File( "/home/paul/workspace/wf-common/test-wf/" ) );
    wfLoader.importWithDependencies( "embedded-task-rej", resolver );



    sess.getTransaction().commit();
  }
}
