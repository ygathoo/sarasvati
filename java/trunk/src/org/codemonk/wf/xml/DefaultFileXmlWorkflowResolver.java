package org.codemonk.wf.xml;

import java.io.File;

import javax.xml.bind.JAXBException;

public class DefaultFileXmlWorkflowResolver implements XmlWorkflowResolver
{
  protected File basePath;
  protected XmlLoader loader;
  
  public DefaultFileXmlWorkflowResolver (XmlLoader loader, File basePath)
  {    
    this.basePath = basePath;
    this.loader = loader;
  }
  
  @Override
  public XmlWorkflow resolve (String name) throws JAXBException
  {
    File defFile = new File( basePath, name + ".wf.xml" );
    return loader.loadWorkflow( defFile );
  }
}
