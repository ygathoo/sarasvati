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

package com.googlecode.sarasvati.xml;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.stream.StreamSource;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.googlecode.sarasvati.load.LoadException;

public class XmlLoader
{
  protected JAXBContext context;
  protected Schema      schema;

  public XmlLoader (JAXBContext context) throws LoadException
  {
    this.context = context;
    loadSchema();
  }

  public XmlLoader (String... extraPackages) throws JAXBException,
      LoadException
  {
    String packages = "com.googlecode.sarasvati.xml";

    if (extraPackages != null)
    {
      for (String p : extraPackages)
        packages += ":" + p;
    }

    this.context = JAXBContext.newInstance( packages );
    loadSchema();
  }

  private void loadSchema () throws LoadException
  {
    InputStream is = getClass().getClassLoader().getResourceAsStream(
        "com/googlecode/sarasvati/ProcessDefinition.xsd" );

    if (is == null)
    {
      is = getClass().getClassLoader().getResourceAsStream(
          "/com/googlecode/sarasvati/ProcessDefinition.xsd" );
    }

    if (is == null)
    {
      throw new LoadException( "ProcessDefinition.xsd not found in classpath" );
    }

    try
    {
      SchemaFactory factory = SchemaFactory
          .newInstance( XMLConstants.W3C_XML_SCHEMA_NS_URI );
      schema = factory.newSchema( new StreamSource( is ) );
    } catch (SAXException se)
    {
      throw new LoadException( "Failed to load schema", se );
    } finally
    {
      try
      {
        is.close();
      } catch (IOException ioe)
      {
        // ignore
      }
    }
  }

  protected Unmarshaller getUnmarshaller () throws JAXBException
  {
    Unmarshaller u = context.createUnmarshaller();
    u.setSchema( schema );
    u.setEventHandler( new javax.xml.bind.helpers.DefaultValidationEventHandler() );
    return u;
  }

  protected Marshaller getMarshaller () throws JAXBException
  {
    Marshaller m = context.createMarshaller();
    m.setSchema( schema );
    m.setEventHandler( new javax.xml.bind.helpers.DefaultValidationEventHandler() );
    m.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, true );
    return m;
  }

  public XmlProcessDefinition loadProcessDefinition (File file)
      throws JAXBException
  {
    return (XmlProcessDefinition) getUnmarshaller().unmarshal( file );
  }

  public XmlProcessDefinition loadProcessDefinition (InputStream in)
      throws JAXBException
  {
    return (XmlProcessDefinition) getUnmarshaller().unmarshal( in );
  }

  public XmlProcessDefinition loadProcessDefinition (Reader in)
      throws JAXBException
  {
    return (XmlProcessDefinition) getUnmarshaller().unmarshal( in );
  }

  public XmlProcessDefinition loadProcessDefinition (InputSource in)
      throws JAXBException
  {
    return (XmlProcessDefinition) getUnmarshaller().unmarshal( in );
  }

  public void saveProcessDefinition (XmlProcessDefinition xmlProcDef, File file)
      throws JAXBException, IOException
  {
    FileOutputStream fOut = new FileOutputStream( file );

    try
    {
      getMarshaller().marshal( xmlProcDef, fOut );
    }
    finally
    {
      fOut.close();
    }
  }
}