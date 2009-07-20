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

    Copyright 2009 Paul Lorenz
*/
package com.googlecode.sarasvati.load;

public class LoadResult
{
  private String name;
  private boolean isNew;
  private String dependency;

  private LoadResult (final String name,
                     final boolean isNew,
                     final String dependency)
  {
    this.name = name;
    this.isNew = isNew;
    this.dependency = dependency;
  }

  public String getName ()
  {
    return name;
  }

  public boolean isNew ()
  {
    return isNew;
  }

  public boolean isCascade ()
  {
    return dependency != null;
  }

  public String getDependency ()
  {
    return dependency;
  }

  public static LoadResult newGraph (final String name)
  {
    return new LoadResult( name, true, null );
  }

  public static LoadResult updatedGraph (final String name)
  {
    return new LoadResult( name, false, null );
  }

  public static LoadResult updatedGraph (final String name, final String dependency)
  {
    return new LoadResult( name, false, dependency );
  }

  @Override
  public String toString ()
  {
    if ( isNew() )
    {
      return name + " loaded for first time";
    }

    if ( !isCascade() )
    {
      return name + " loaded because it changed on the file system";
    }

    return name + " loaded because a dependency was reloaded: " + dependency;
  }
}