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
package com.googlecode.sarasvati.load.properties;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.util.SvUtil;

public class DOMToObjectLoadHelper
{
  public static Map<String, String> setBeanValues (Object obj, Node node) throws LoadException
  {
    Map<String, String> beanProperties = new HashMap<String, String>();
    setBeanValues( obj, node, null, beanProperties );
    return beanProperties;
  }

  public static void setBeanValues (Object obj, Node node, String name, Map<String, String> beanProperties) throws LoadException
  {
    PropertyMutator editor = getMutatorForProperty( obj, node.getLocalName() );

    Object currentValue = editor.getCurrentValue();

    NodeList list = node.getChildNodes();

    boolean hasElementChildren = false;

    for ( int i = 0; i < list.getLength(); i++ )
    {
      Object child = list.item( i );
      if ( child instanceof Element )
      {
        Element elemChild = (Element)child;
        String childName = getChildName( name, node.getLocalName() );
        setBeanValues( currentValue, elemChild, childName, beanProperties );
        hasElementChildren = true;
      }
    }

    if ( !hasElementChildren )
    {
      String value = node.getTextContent();

      if ( !SvUtil.isBlankOrNull( value ) )
      {
        editor.setFromText( value.trim() );
        beanProperties.put( getChildName( name, node.getLocalName() ), value );
      }
    }

    NamedNodeMap attrs = node.getAttributes();

    for ( int i = 0; attrs != null && i < attrs.getLength(); i++ )
    {
      Attr attribute = (Attr)attrs.item( i );
      String attrName = attribute.getLocalName();

      if ( attrName.equals( "xmlns" ) || attribute.getName().startsWith( "xmlns:" ) )
      {
        continue;
      }

      if ( hasElementChildren )
      {
        String childName = getChildName( name, attrName );
        setBeanValues( currentValue, attribute, childName, beanProperties );
      }
      else
      {
        // If we have something of the form
        // <foo bar="test">
        //   some stuff
        // </foo>
        // we want to set the 'foo' property to 'some stuff' and
        // set the fooBar property to 'test'
        String propertyName = node.getLocalName() + Character.toUpperCase( attrName.charAt( 0 ) ) + attrName.substring( 1 );
        String value = attribute.getNodeValue();
        getMutatorForProperty( obj, propertyName ).setFromText( value );
        beanProperties.put( getChildName( name, propertyName ), value );
      }
    }
  }

  private static String getChildName (String prefix, String name)
  {
    return prefix == null ? name : prefix + "." + name;
  }

  public static PropertyMutator getMutatorForProperty (Object obj, String name) throws LoadException
  {
    BeanInfo beanInfo = null;

    try
    {
      beanInfo = Introspector.getBeanInfo( obj.getClass() );
    }
    catch( IntrospectionException ie )
    {
      throw new LoadException( "Could not introspect obj " + obj, ie );
    }

    PropertyDescriptor attr = null;

    for ( PropertyDescriptor pd : beanInfo.getPropertyDescriptors() )
    {
      if ( pd.getName().equals( name ) )
      {
        attr = pd;
        break;
      }
    }

    if ( attr == null )
    {
      throw new LoadException( obj.getClass().getName() + " has no attribute named " + name );
    }

    return PropertyMutatorRegistry.getMutator( attr, obj, new BasePropertyMutator() );
  }

  public static void setValues (Object obj, Map<String, String> values) throws LoadException
  {
    for (Entry<String, String> entry : values.entrySet() )
    {
      PropertyMutator mutator = null;
      Object target = obj;
      for ( String prop : entry.getKey().split( "\\." ) )
      {
        mutator = getMutatorForProperty( target, prop );
        target = mutator.getCurrentValue();
      }
      mutator.setFromText( entry.getValue() );
    }
  }
}