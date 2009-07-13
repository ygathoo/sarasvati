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

import java.util.prefs.Preferences;

public class EditorPreferences
{
  private static final String LIBRARY_PATH_KEY = "libraryPath";
  private static final String RECURSE_LIBRARY_KEY = "recurseLibrary";
  protected String libraryPath;

  protected boolean recurseLibrary;

  public void load ()
  {
    Preferences prefs = Preferences.userNodeForPackage( getClass() );
    libraryPath = prefs.get( LIBRARY_PATH_KEY, null );
    recurseLibrary = prefs.getBoolean( RECURSE_LIBRARY_KEY, false );
  }
}
