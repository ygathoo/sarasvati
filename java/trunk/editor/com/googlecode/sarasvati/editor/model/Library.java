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
import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import com.googlecode.sarasvati.load.definition.ProcessDefinition;
import com.googlecode.sarasvati.util.FileVisitor;
import com.googlecode.sarasvati.util.SvUtil;

public class Library
{
  private static final Library INSTANCE = new Library ();

  public static Library getInstance ()
  {
    return INSTANCE;
  }

  private Map<String, LibraryEntry> entries;
  private File basePath;

  public Library ()
  {
    emptyLibrary();
  }

  public void loadFromPath (final String path,
                            final boolean recurse)
  {
    entries = new TreeMap<String, LibraryEntry>();
    basePath = new File( path );

    FileVisitor visitor = new FileVisitor()
    {

      @Override
      public boolean accept (final File dir, final String name)
      {
        return name.endsWith( ".wf.xml" );
      }

      @Override
      public void accept (final File file)
      {
        String name = file.getName();
        name = name.substring( 0, name.length() - 7 );
        entries.put( name, new LibraryEntry( name, file ) );
      }
    };

    SvUtil.visitRecursive( basePath, visitor, recurse );
  }

  public void emptyLibrary ()
  {
    entries = new TreeMap<String, LibraryEntry>();
    basePath = new File( ".") ;
  }

  public Collection<LibraryEntry> getEntries ()
  {
    return entries.values();
  }

  public Collection<String> getNames ()
  {
    return entries.keySet();
  }

  public LibraryEntry getEntry (final String name)
  {
    return entries.get( name );
  }

  /**
   * @return the basePath
   */
  public File getBasePath ()
  {
    return basePath;
  }

  public ProcessDefinition getProcessDefinition (final String name)
  {
    LibraryEntry entry = getEntry( name );
    return entry == null ? null :entry.getProcessDefinition();
  }

  public void update (final ProcessDefinition processDefinition, final File path)
  {
    entries.put( processDefinition.getName(), new LibraryEntry( processDefinition, path ) );
  }
}