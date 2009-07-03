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
package com.googlecode.sarasvati.editor.dialog;

import java.awt.Point;

import javax.swing.JDialog;
import javax.swing.JFrame;

import com.googlecode.sarasvati.editor.model.EditorArc;
import com.googlecode.sarasvati.editor.model.EditorNode;

public class DialogFactory
{
  protected static JFrame frame = null;

  public static void setFrame (JFrame frame)
  {
    DialogFactory.frame = frame;
  }

  public static JDialog newNodePropertiesDialog (EditorNode node)
  {
    return new NodePropertiesDialog( frame, node );
  }

  public static JDialog newArcPropertiesDialog (EditorArc arc)
  {
    return new ArcPropertiesDialog( frame, arc );
  }

  public static JDialog newPreferencesDialog ()
  {
    PreferencesDialog dialog = new PreferencesDialog( frame );
    Point location = frame.getLocation();
    dialog.setLocation( location.x + 50, location.y + 10 );
    return dialog;
  }
}
