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

package org.codemonk.wf.mem;

import java.util.HashMap;
import java.util.Map;

import org.codemonk.wf.Node;
import org.codemonk.wf.NodeToken;
import org.codemonk.wf.Process;

public class MemNodeToken implements NodeToken
{
  protected long id;
  protected Node node;
  protected MemProcess process;

  protected Map<String, String> attributes = new HashMap<String, String>();

  public MemNodeToken( Node node, MemProcess process )
  {
    this.id = process.nextTokenId();
    this.node = node;
    this.process = process;
  }

  @Override
  public boolean getBooleanAttribute (String name)
  {
    return Boolean.valueOf( attributes.get( name ) );
  }

  @Override
  public long getLongAttribute (String name)
  {
    String val = attributes.get( name );

    if ( val == null )
    {
      return 0;
    }

    try
    {
      return Long.parseLong( val );
    }
    catch (NumberFormatException nfe)
    {
      return 0;
    }
  }

  @Override
  public Node getNode()
  {
    return node;
  }

  @Override
  public Process getProcess()
  {
    return process;
  }

  @Override
  public String getStringAttribute (String name)
  {
    return attributes.get( name );
  }

  @Override
  public boolean hasAttribute( String name )
  {
    return attributes.containsKey( name );
  }

  @Override
  public void removeAttribute( String name )
  {
    attributes.remove( name );
  }

  @Override
  public void setBooleanAttribute (String name, boolean value)
  {
    attributes.put( name, String.valueOf( value ) );
  }

  @Override
  public void setLongAttribute (String name, long value)
  {
    attributes.put( name, String.valueOf( value ) );
  }

  @Override
  public void setStringAttribute (String name, String value)
  {
    attributes.put( name, value );
  }

  @Override
  public Iterable<String> getAttributeNames()
  {
    return attributes.keySet();
  }

  @Override
  public void markComplete ()
  {
    /** Does nothing */
  }

  @Override
  public int hashCode ()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + (int)( id ^ ( id >>> 32 ) );
    return result;
  }

  @Override
  public boolean equals (Object obj)
  {
    if ( this == obj ) return true;
    if ( obj == null ) return false;
    if ( !( obj instanceof MemNodeToken ) ) return false;
    final MemNodeToken other = (MemNodeToken)obj;
    if ( id != other.id ) return false;
    return true;
  }
}