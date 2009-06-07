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

import java.util.List;

/**
 * An TokenSetEnv stores attributes related to a token set and it's member tokens.
 * Unlike a normal {@link Env}, for each attribute, it may store a many values,
 * each with a different member index.  
 * 
 * @author Paul Lorenz
 */
public interface TokenSetEnv extends Env
{
  /**
   * Gets the attributes as a String. If there is no value set for
   * the attribute, null will be returned.
   *
   * @param index The index of the attribute. 
   * @param name The name of the attribute to get
   * @return The value of attribute or null if no value is set for the attribute.
   *
   */
  List<String> getStringAttributes (String name);

  /**
   * Sets the attribute of the given name to the given string value.
   *
   * @param name The name of the attribute to set.
   * @param value The value to set the attribute to
   */
  void setStringAttributes (String name, List<String> values);

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
  List<Long> getLongAttributes (String name);

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

  /**
   * Returns an Iterable of attribute names.
   *
   * @return Iterable of attribute names
   */
  Iterable<String> getAttributeNames ();

  /**
   * Set a transient attribute of the environment. The attribute will
   * only be in the environment as long as the associated {@link GraphProcess} or
   * {@link NodeToken} instance is in memory. In other words, these
   * attributes will not be persisted to whatever the backing store
   * is.
   *
   * <br/>
   * If a transient value already existed for the given name, the value
   * will be overwritten
   * <br/>
   *
   * In the case of a memory back implementation, these transient
   * attributes will have the same lifetime as other attributes.
   *
   * @param name The name of the attribute
   * @param value The value of the attribute
   */
  void setTransientAttribute (String name, Object value);

  /**
   * Returns true if this transient attribute exists in the environment
   * and false otherwise.
   *
   * @param name The transient attribute name
   * @return True if the given transient attribute exists in this environment
   */
  boolean hasTransientAttribute (String name);

  /**
   * Returns the value associated with the given named transient attribute.
   *
   * @param name The attribute name
   * @return The attribute value, or null if a value has not been set.
   */
  Object getTransientAttribute (String name);

  /**
   * Remove the given attribute from the environment.
   *
   * @param name The attribute name
   */
  void removeTransientAttribute (String name);

  /**
   * Returns an {@link Iterable} of transient attribute names.
   *
   * @return {@link Iterable} of transient attribute names
   */
  Iterable<String> getTransientAttributeNames ();

  /**
   * Copy all attributes from given env, including transient attributes.
   *
   * @param env The environment to copy
   */
  void importEnv (TokenSetEnv env);
}