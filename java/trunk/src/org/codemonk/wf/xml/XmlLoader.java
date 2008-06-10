/**
 * Created on Jun 9, 2008
 */
package org.codemonk.wf.xml;

import java.io.File;
import java.io.InputStream;
import java.io.Reader;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.xml.sax.InputSource;

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

  public XmlWorkflow loadWorkflow (File file) throws JAXBException
  {
    return (XmlWorkflow)getUnmarshaller().unmarshal( file );
  }

  public XmlWorkflow loadWorkflow (InputStream in) throws JAXBException
  {
    return (XmlWorkflow)getUnmarshaller().unmarshal( in );
  }
  
  public XmlWorkflow loadWorkflow (Reader in) throws JAXBException
  {
    return (XmlWorkflow)getUnmarshaller().unmarshal( in );
  }
  
  public XmlWorkflow loadWorkflow (InputSource in) throws JAXBException
  {
    return (XmlWorkflow)getUnmarshaller().unmarshal( in );
  }  
}