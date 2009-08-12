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

    Copyright 2008-2009 Paul Lorenz
*/
package com.googlecode.sarasvati.editor.model;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class EditorGraph
{
  protected File                 file;
  protected String               name;

  protected List<EditorNode>     nodes     = new ArrayList<EditorNode>();
  protected List<EditorExternal> externals = new ArrayList<EditorExternal>();
  protected List<EditorArc>      arcs      = new ArrayList<EditorArc>();

  protected Map<EditorGraphMember<?>, List<EditorArc>> outArcs = new HashMap<EditorGraphMember<?>, List<EditorArc>> ();

  public String getName ()
  {
    return name;
  }

  public void setName (final String name)
  {
    this.name = name;
  }

  public File getFile ()
  {
    return file;
  }

  public void setFile (final File file)
  {
    this.file = file;
  }

  public List<EditorNode> getNodes()
  {
    return nodes;
  }

  public void setNodes (final List<EditorNode> nodes)
  {
    this.nodes = nodes;
  }

  public void addNode (final EditorNode node)
  {
    nodes.add( node );
  }

  public void removeNode (final EditorNode node)
  {
    nodes.remove( node );
  }

  public List<EditorExternal> getExternals()
  {
    return externals;
  }

  public void setExternals (final List<EditorExternal> externals )
  {
    this.externals = externals;
  }

  public void addExternal (final EditorExternal external)
  {
    externals.add( external );
  }

  public void removeExternal (final EditorExternal external)
  {
    externals.remove( external );
  }

  public List<EditorArc> getArcs()
  {
    return arcs;
  }

  public void setArcs (final List<EditorArc> arcs )
  {
    this.arcs = arcs;
  }

  public List<EditorArc> getOutArcs (final EditorGraphMember<?> member)
  {
    List<EditorArc> list = outArcs.get( member );
    if ( list == null )
    {
      list = new LinkedList<EditorArc>();
      outArcs.put( member, list );
    }
    return list;
  }

  public void addArc (final EditorArc arc)
  {
    arcs.add( arc );
    getOutArcs( arc.getStart() ).add( arc );
  }

  public void removeArc (final EditorArc arc)
  {
    arcs.remove( arc );
    getOutArcs( arc.getStart() ).remove( arc );
  }

  public Set<String> getUniqueNames (final Collection<? extends EditorGraphMember<?>> members)
  {
    Set<String> names = new HashSet<String> ();
    for ( EditorGraphMember<?> member : members )
    {
      names.add( member.getName() );
    }
    return names;
  }

  public Set<String> getCurrentNodeNames ()
  {
    return getUniqueNames( nodes );
  }

  public Set<String> getCurrentExternalNames ()
  {
    return getUniqueNames( externals );
  }

  public List<String> validateGraph ()
  {
    List<String> errors = new LinkedList<String> ();

    Set<String> nodeIds = new HashSet<String> ();

    for ( EditorNode node : nodes )
    {
      String nodeName = node.getState().getName();
      if (nodeIds.contains( nodeName ) )
      {
        errors.add( "Node name '" + nodeName + "' is used more than once. Each node must have a unique name." );
      }
      nodeIds.add( nodeName );
    }

    return errors;
  }
}