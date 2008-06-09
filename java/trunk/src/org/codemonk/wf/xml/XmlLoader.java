/**
 * Created on Jun 9, 2008
 */
package org.codemonk.wf.xml;

import java.io.File;
import java.io.InputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

public class XmlLoader
{
  protected JAXBContext context;

  public XmlLoader (JAXBContext context)
  {
    this.context = context;
  }

  public XmlLoader (Class<?>...classes) throws JAXBException
  {
    Class<?>[] baseClasses = { XmlWorkflow.class };

    if ( classes == null )
    {
      this.context = JAXBContext.newInstance( baseClasses );
    }
    else
    {
      Class<?>[] xmlClasses = new Class[classes.length + baseClasses.length];
      System.arraycopy( baseClasses, 0, xmlClasses, 0, baseClasses.length );
      System.arraycopy( classes, 0, xmlClasses, baseClasses.length, classes.length );
      this.context = JAXBContext.newInstance( xmlClasses );
    }
  }

  protected Unmarshaller getUnmarshaller () throws JAXBException
  {
    Unmarshaller u = context.createUnmarshaller();
    u.setEventHandler(new javax.xml.bind.helpers.DefaultValidationEventHandler());
    return u;
  }

  public void loadWorkflow (File file) throws JAXBException
  {
    XmlWorkflow wf = (XmlWorkflow)getUnmarshaller().unmarshal( file );
  }

  public void loadWorkflow (InputStream in) throws JAXBException
  {
    XmlWorkflow wf = (XmlWorkflow)getUnmarshaller().unmarshal( in );
  }
}