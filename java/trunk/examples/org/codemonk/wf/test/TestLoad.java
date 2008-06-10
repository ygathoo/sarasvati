/**
 * Created on Jun 9, 2008
 */
package org.codemonk.wf.test;

import java.io.File;

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
    XmlWorkflowResolver resolver = new DefaultFileXmlWorkflowResolver(xmlLoader, new File( "/home/paul/workspace/wf-common/test-wf/" ) );
    wfLoader.importWithDependencies( "embedded-task-rej", resolver );
    
    sess.getTransaction().commit();
  }
}
