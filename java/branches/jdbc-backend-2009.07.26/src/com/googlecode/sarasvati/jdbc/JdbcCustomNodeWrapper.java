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

package com.googlecode.sarasvati.jdbc;

import com.googlecode.sarasvati.CustomNode;
import com.googlecode.sarasvati.CustomNodeWrapper;
import com.googlecode.sarasvati.Engine;
import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.NodeToken;
import com.googlecode.sarasvati.load.properties.DOMToObjectLoadHelper;

public class JdbcCustomNodeWrapper extends JdbcPropertyNode implements CustomNodeWrapper
{
  protected CustomNode customNode;

  public JdbcCustomNodeWrapper () { /* Default constructor for Hibernate */ }

  public JdbcCustomNodeWrapper (CustomNode customNode)
  {
    this.customNode = customNode;
    customNode.setNodeWrapper( this );
  }

  public CustomNode getCustomNode (Engine engine)
  {
    return customNode;
  }

  @Override
  public GuardResponse defaultGuard (Engine engine, NodeToken token)
  {
    return super.guard( engine, token );
  }

  @Override
  public <T> T getDefaultAdaptor (Class<T> clazz)
  {
    return super.getAdaptor( clazz );
  }

  @Override
  public void execute (Engine engine, NodeToken token)
  {
    getCustomNode( engine ).execute( engine, token );
  }

  @Override
  public void backtrack (Engine engine, NodeToken token)
  {
    getCustomNode( engine ).backtrack( engine, token );
  }

  @Override
  public boolean isBacktrackable (Engine engine, NodeToken token)
  {
    return getCustomNode( engine ).isBacktrackable( engine, token );
  }

  @Override
  public void afterLoad (final JdbcEngine engine)
  {
    super.afterLoad( engine );
    DOMToObjectLoadHelper.setValues( customNode, attrMap );
  }
}