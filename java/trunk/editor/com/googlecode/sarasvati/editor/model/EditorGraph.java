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
package com.googlecode.sarasvati.editor.model;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class EditorGraph
{
  protected File                 file;
  protected String               name;

  protected List<EditorNode>     nodes = new ArrayList<EditorNode>();
  protected List<EditorExternal> externals = new ArrayList<EditorExternal>();
  protected List<EditorArc>      arcs    = new ArrayList<EditorArc>();

  public String getName ()
  {
    return name;
  }

  public void setName (String name)
  {
    this.name = name;
  }

  public File getFile ()
  {
    return file;
  }

  public void setFile (File file)
  {
    this.file = file;
  }

  public List<EditorNode> getNodes()
  {
    return nodes;
  }

  public void setNodes (List<EditorNode> nodes)
  {
    this.nodes = nodes;
  }

  public void addNode (EditorNode node)
  {
    nodes.add( node );
  }

  public void removeNode (EditorNode node)
  {
    nodes.remove( node );
  }

  public List<EditorExternal> getExternals()
  {
    return externals;
  }

  public void setExternals( List<EditorExternal> externals )
  {
    this.externals = externals;
  }

  public void addExternal (EditorExternal external)
  {
    externals.add( external );
  }

  public List<EditorArc> getArcs()
  {
    return arcs;
  }

  public void setArcs( List<EditorArc> arcs )
  {
    this.arcs = arcs;
  }

  public void addArc (EditorArc arc)
  {
    arcs.add( arc );
  }

  public void removeArc (EditorArc arc)
  {
    arcs.remove( arc );
  }
}