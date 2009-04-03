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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class EditorGraph
{
  protected File                           file;
  protected String                         name;

  protected Map<String, EditorGraphMember> members = new HashMap<String, EditorGraphMember>();
  protected List<EditorArc> arcs = new ArrayList<EditorArc>();

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

  public Map<String, EditorGraphMember> getMembers ()
  {
    return members;
  }

  public void setMembers (Map<String, EditorGraphMember> members)
  {
    this.members = members;
  }

  public void addMember (EditorGraphMember member)
  {
    members.put( member.getName(), member );
  }

  public boolean hasMember (String memberName)
  {
    return members.containsKey( memberName );
  }

  public List<EditorArc> getArcs()
  {
    return arcs;
  }

  public void setArcs( List<EditorArc> arcs )
  {
    this.arcs = arcs;
  }

  public void addArc (String start, String end, String label)
  {
    EditorGraphMember startMember = members.get( start );
    EditorGraphMember endMember = members.get( end );

    EditorArc arc = new EditorArc();
    arc.setStart(  startMember );
    arc.setEnd( endMember );
    arc.setLabel( label );

    arcs.add( arc );
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