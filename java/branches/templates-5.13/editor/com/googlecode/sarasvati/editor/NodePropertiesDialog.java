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

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.BevelBorder;

import com.googlecode.sarasvati.editor.model.EditorNode;

public class NodePropertiesDialog extends JDialog
{
  private static final long serialVersionUID = 1L;

  protected EditorNode node;

  protected JTextField nameInput;
  protected JCheckBox  isStartInput;
  protected JCheckBox  isJoinInput;

  public NodePropertiesDialog (JFrame frame, EditorNode node)
  {
    super( frame, "Node Properties", false );
    this.node = node;

    setUndecorated( true );

    JPanel panel = new JPanel();
    panel.setBorder( new BevelBorder( BevelBorder.RAISED ) );
    BoxLayout layout = new BoxLayout( panel, BoxLayout.PAGE_AXIS );
    panel.setLayout( layout );
    getContentPane().add( panel );

    nameInput = new JTextField( node.getName(), 20 );

    JLabel nameLabel = new JLabel( "Name" );
    nameLabel.setLabelFor( nameInput );

    JPanel namePanel = new JPanel( new FlowLayout() );
    namePanel.add( nameLabel );
    namePanel.add( nameInput );

    isStartInput = new JCheckBox( "Start node", node.isStart() );
    isJoinInput = new JCheckBox( "Join node", false );

    JButton cancelButton = new JButton( "Cancel" );

    cancelButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed (ActionEvent e)
      {
        setVisible( false );
      }
    });

    JButton applyButton = new JButton( "Apply" );

    JPanel buttonPanel = new JPanel( new FlowLayout( FlowLayout.CENTER, 15, 5 ) );
    buttonPanel.add( cancelButton );
    buttonPanel.add( applyButton );

    panel.add( namePanel );
    panel.add( isStartInput );
    panel.add( isJoinInput );
    panel.add( buttonPanel );

    if ( isAlwaysOnTopSupported() )
    {
      setAlwaysOnTop( true );
    }
    pack();
  }
}
