package com.googlecode.sarasvati.editor;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.KeyStroke;

public class OpenAction extends AbstractAction
{
  private static final long serialVersionUID = 1L;

  protected GraphEditor editor;

  public OpenAction (GraphEditor editor)
  {
    super( "Open" );
    this.editor = editor;

    putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_O, KeyEvent.CTRL_MASK ) );
    putValue( Action.MNEMONIC_KEY, KeyEvent.VK_O );
  }

  @Override
  public void actionPerformed (ActionEvent e)
  {
    JFileChooser fileChooser = new JFileChooser();
    int retVal = fileChooser.showOpenDialog( editor.getMainWindow() );

    if ( retVal == JFileChooser.APPROVE_OPTION )
    {
      editor.openProcessDefinition( fileChooser.getSelectedFile() );
    }
  }
}