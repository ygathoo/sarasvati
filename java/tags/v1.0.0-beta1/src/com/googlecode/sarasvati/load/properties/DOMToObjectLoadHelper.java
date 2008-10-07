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

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.googlecode.sarasvati.load.LoadException;

public class DOMToObjectLoadHelper
{
  public static void setBeanValues (Object obj, Node node) throws LoadException
  {
    PropertyMutator editor = getMutatorForNode( obj, node.getLocalName() );

    Object currentValue = editor.getCurrentValue();

    NodeList list = node.getChildNodes();

    boolean hasElementChildren = false;

    for ( int i = 0; i < list.getLength(); i++ )
    {
      Object child = list.item( i );
      if ( child instanceof Element )
      {
        setBeanValues( currentValue, (Element)child );
        hasElementChildren = true;
      }
    }

    if ( !hasElementChildren )
    {
      String value = node.getTextContent();
      value = value == null ? null : value.trim();
      editor.setFromText( value );
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
        setBeanValues( currentValue, attribute );
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
        getMutatorForNode( obj, propertyName ).setFromText( attribute.getNodeValue() );
      }
    }
  }

  public static PropertyMutator getMutatorForNode (Object obj, String name) throws LoadException
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
}
