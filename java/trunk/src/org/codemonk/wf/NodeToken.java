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
/**
 * Created on Apr 25, 2008
 */
package org.codemonk.wf;

/**
 * Node tokens point to nodes in the graph. Unlike arc tokens,
 * they may have attributes associated with them.
 *
 * @author Paul Lorenz
 */
public interface NodeToken extends Token
{
  /**
   * @return The node that this token points to.
   */
  Node getNode ();

  /**
   * Returns the process that this node token belongs to.
   *
   * @return The associated process
   */
  Process getProcess ();

  /**
   * Gets an attribute as a String. If there is no value set for
   * the attribute, null will be returned.
   *
   * @param name The name of the attribute to get
   * @return The value of attribute or null if no value is set for the attribute.
   *
   */
  String getStringAttribute (String name);

  /**
   * Sets the attribute of the given name to the given string value.
   *
   * @param name The name of the attribute to set.
   * @param value The value to set the attribute to
   */
  void setStringAttribute (String name, String value);

  /**
   * Gets an attribute as a long. If there is no value set for
   * the attribute or if the attribute can not be read as a long,
   * 0 will be returned.
   *
   * @param name The name of the attribute to get
   * @return The value of attribute or 0 if no value is set for the attribute or
   *         the value cannot be interpreted as a long
   *
   */
  long getLongAttribute (String name);

  /**
   * Sets the attribute of the given name to the given long value.
   *
   * @param name The name of the attribute to set.
   * @param value The value to set the attribute to
   */
  void setLongAttribute (String name, long value);

  /**
   * Gets an attribute as a boolean. If there is no value set for
   * the attribute or if the attribute can not be read as a boolean,
   * false will be returned.
   *
   * @param name The name of the attribute to get
   * @return The value of attribute or false if no value is set for the attribute or
   *         the value cannot be interpreted as a boolean.
   *
   */
  boolean getBooleanAttribute (String name);

  /**
   * Sets the attribute of the given name to the given boolean value.
   *
   * @param name The name of the attribute to set.
   * @param value The value to set the attribute to
   */
  void setBooleanAttribute (String name, boolean value);

  /**
   * Unsets any attribute with the given name.
   *
   * @param name The name of the attribute to remove
   */
  void removeAttribute (String name);

  /**
   * Checks if the given attribute is present.
   *
   * @param name Name of the attribute to check for
   * @return True if the given attribute is present, false otherwise.
   */
  boolean hasAttribute (String name);
}
