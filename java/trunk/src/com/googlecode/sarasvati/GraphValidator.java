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
package com.googlecode.sarasvati;

import com.googlecode.sarasvati.load.GraphRepository;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.xml.XmlArc;
import com.googlecode.sarasvati.xml.XmlExternal;
import com.googlecode.sarasvati.xml.XmlExternalArc;
import com.googlecode.sarasvati.xml.XmlNode;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

/**
 * Interface containing methods which are invoked as part of the graph
 * loading process. The methods may throw exceptions if the graph
 * violates some user constraint.
 *
 * @author Paul Lorenz
 */
public interface GraphValidator
{
  /**
   * Allows user validation of the xml process definition. This is invoked at the very
   * beginning of the load process, after the XML has been loaded, but before {@link Graph}
   * construction begins.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param xmlProcessDefinition The {@link XmlProcessDefinition} to be validated.
   */
  void validateXmlProcessDefinition (XmlProcessDefinition xmlProcessDefinition) throws LoadException;

  /**
   * Allows user validation of each {@link XmlExternal} in the {@link XmlProcessDefinition} being loaded.
   * This is invoked at the very beginning of the load process, after the XML has been loaded, but
   * before {@link Graph} construction begins.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param xmlExternal The {@link XmlExternal} to be validated.
   */
  void validateXmlExternal (XmlExternal xmlExternal) throws LoadException;

  /**
   * Allows user validation of each {@link XmlNode} in the {@link XmlProcessDefinition} being loaded.
   * This is invoked at the very beginning of the load process, after the XML has been loaded, but
   * before {@link Graph} construction begins.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param xmlNode The {@link XmlNode} to be validated.
   */
  void validateXmlNode (XmlNode xmlNode) throws LoadException;

  /**
   * Allows user validation of each {@link XmlArc} in the {@link XmlProcessDefinition} being loaded.
   * This is invoked at the very beginning of the load process, after the XML has been loaded, but
   * before {@link Graph} construction begins.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param xmlArc The {@link XmlArc} to be validated.
   */
  void validateXmlArc (XmlArc xmlArc) throws LoadException;

  /**
   * Allows user validation of each {@link XmlExternalArc} in the {@link XmlProcessDefinition} being loaded.
   * This is invoked at the very beginning of the load process, after the XML has been loaded, but
   * before {@link Graph} construction begins.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param xmlExternalArc The {@link XmlExternalArc} to be validated.
   */
  void validateXmlExternalArc (XmlExternalArc xmlExternalArc) throws LoadException;

  /**
   * Allows user validation of the loaded {@link Graph}, before it is added to the {@link GraphRepository}.
   * This is invoked at the very end of the load process, after the XML has been loaded, and after the
   * full {@link Graph} has been constructed.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param graph The {@link Graph} to be validated.
   */
  void validateGraph (Graph graph) throws LoadException;

  /**
   * Allows user validation of each {@link Node} in the loaded {@link Graph}.
   * This is invoked at the very end of the load process, after the XML has
   * been loaded, and after the full {@link Graph} has been constructed.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param node The {@link Node} to be validated.
   */
  void validateNode (Node node) throws LoadException;

  /**
   * Allows user validation of each {@link Arc} in the loaded {@link Graph}.
   * This is invoked at the very end of the load process, after the XML has
   * been loaded, and after the full {@link Graph} has been constructed.
   * <p>
   * If a user constraint is violated, a {@link LoadException} should be thrown.
   *
   * @param graph The {@link Arc} to be validated.
   */
  void validateArc (Arc arc) throws LoadException;
}