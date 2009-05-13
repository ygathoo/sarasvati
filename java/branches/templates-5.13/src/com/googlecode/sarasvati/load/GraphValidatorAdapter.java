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
package com.googlecode.sarasvati.load;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.xml.XmlArc;
import com.googlecode.sarasvati.xml.XmlExternal;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

/**
 * Provides a base for creating {@link GraphValidator} implementations in
 * cases where not all methods will be overridden. Provides default, do-nothing
 * implementations of all methods.
 *
 * @author Paul Lorenz
 */
public class GraphValidatorAdapter implements GraphValidator
{
  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateArc (Arc arc) throws LoadException
  {
    // Does nothing
  }

  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateGraph (Graph graph) throws LoadException
  {
    // Does nothing
  }

  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateNode (Node node) throws LoadException
  {
    // Does nothing
  }

  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateXmlArc (XmlArc xmlArc) throws LoadException
  {
    // Does nothing
  }

  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateXmlExternal (XmlExternal xmlExternal)
      throws LoadException
  {
    // Does nothing
  }

  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateXmlExternalArc (XmlExternalArc xmlExternalArc)
      throws LoadException
  {
    // Does nothing
  }

  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateXmlNode (XmlNode xmlNode) throws LoadException
  {
    // Does nothing
  }

  /**
   * Does nothing. Override in a subclass to do user constraint checking
   */
  @Override
  public void validateXmlProcessDefinition (XmlProcessDefinition xmlProcessDefinition)
    throws LoadException
  {
    // Does nothing
  }
}
