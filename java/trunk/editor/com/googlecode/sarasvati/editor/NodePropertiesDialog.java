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
package com.googlecode.sarasvati.editor;

import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.border.BevelBorder;

import com.googlecode.sarasvati.editor.model.EditorNode;

public class NodePropertiesDialog extends JDialog
{
  private static final long serialVersionUID = 1L;

  protected EditorNode node;

  public NodePropertiesDialog (JFrame frame, EditorNode node)
  {
    super( frame, "Node Properties", false );
    this.node = node;

    setUndecorated( true );

    JPanel panel = new NodePropertiesPanel();
    panel.setBorder( new BevelBorder( BevelBorder.RAISED ) );
    BoxLayout layout = new BoxLayout( panel, BoxLayout.PAGE_AXIS );
    panel.setLayout( layout );
    getContentPane().add( panel );

    if ( isAlwaysOnTopSupported() )
    {
      setAlwaysOnTop( true );
    }
    pack();
  }
}
