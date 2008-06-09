/**
 * Created on Jun 9, 2008
 */
package org.codemonk.wf.test;

import java.io.File;

import org.codemonk.wf.xml.XmlLoader;

public class TestLoad
{
  public static void main (String[] args) throws Exception
  {
    XmlLoader loader = new XmlLoader( XmlTaskDef.class );
    loader.loadWorkflow( new File( "/home/paul/workspace/wf-common/test-wf/embedded-task-rej.wf.xml" ) );
  }
}
