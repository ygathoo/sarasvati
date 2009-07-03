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
package com.googlecode.sarasvati.editor.dialog;

import com.googlecode.sarasvati.editor.panel.NodePropertiesPanel;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.border.BevelBorder;

import com.googlecode.sarasvati.editor.model.EditorNode;

public class NodePropertiesDialog extends JDialog
{
  private static final long serialVersionUID = 1L;

  public NodePropertiesDialog (final JFrame frame,
                               final EditorNode node)
  {
    super( frame, node.getState().getName() + " properties", false );

    setUndecorated( false );

    NodePropertiesPanel panel = new NodePropertiesPanel();
    panel.setup( this, node );
    panel.setBorder( new BevelBorder( BevelBorder.RAISED ) );
    getContentPane().add( panel );

    if ( isAlwaysOnTopSupported() )
    {
      setAlwaysOnTop( true );
    }
    pack();
  }
}
