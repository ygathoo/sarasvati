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
package com.googlecode.sarasvati.load.properties;

import com.googlecode.sarasvati.load.LoadException;

public class CharPropertyMutator extends BasePropertyMutator
{
  @Override
  public void setFromText (String text) throws LoadException
  {
    try
    {
      setValue( text == null || text.length() == 0 ? null : text.charAt( 0 ) );
    }
    catch (NumberFormatException nfe)
    {
      throw new LoadException( "Unable to parse value '" + text + "' to an integer value", nfe );
    }
  }
}
