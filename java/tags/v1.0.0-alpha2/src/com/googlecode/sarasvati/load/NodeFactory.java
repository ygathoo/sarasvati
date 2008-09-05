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

import com.googlecode.sarasvati.Node;

public interface NodeFactory
{
  /**
   * Creates a new node of the given type
   * @return The new node
   *
   * @throws LoadException If a node of the given type cannot be instantiated
   */
  Node newNode (String type) throws LoadException;

  /**
   * Loads the custom data into the node
   *
   * @param node The node to set the data in
   * @param custom The custom data from the XML file
   *
   * @throws LoadException If custom data is incorrect.
   */
  void loadCustom (Node node, Object custom) throws LoadException;
}
