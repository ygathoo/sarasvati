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
 * The set of tokens in a process represent the current state
 * of the workflow. There are two types of tokens, node tokens
 * and arc tokens. Node tokens point at nodes and arc tokens
 * point to arcs. Tokens are never moved. They may be marked
 * as complete and new tokens will be created in the areas of
 * the workflow now in progress.
 *
 * @author Paul Lorenz
 */
public interface Token
{
  void markComplete ();
}
