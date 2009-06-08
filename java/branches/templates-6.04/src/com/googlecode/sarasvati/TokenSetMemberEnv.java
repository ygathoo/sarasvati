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

package com.googlecode.sarasvati;

import java.util.List;

public interface TokenSetMemberEnv
{
  String getAttribute (final int memberIndex, final String name);

  <T> T getAttribute (final int memberIndex, final String name, final Class<T> type);

  Iterable<String> getAttributeNames (final int memberIndex);

  boolean hasAttribute (final int memberIndex, final String name);

  void setAttribute (final int memberIndex, final String name, final String value);

  void setAttribute (final int memberIndex, final String name, final Object value);

  void setAttribute (final String name, final List< ? > values);

  void removeAttribute (final int memberIndex, final String name);

  void removeAttribute (final String name);

  Object getTransientAttribute (final int memberIndex, final String name);

  Iterable<String> getTransientAttributeNames (final int memberIndex);

  boolean hasTransientAttribute (final int memberIndex, final String name);

  void removeTransientAttribute (final int memberIndex, final String name);

  void setTransientAttribute (final int memberIndex, final String name, final Object value);

}