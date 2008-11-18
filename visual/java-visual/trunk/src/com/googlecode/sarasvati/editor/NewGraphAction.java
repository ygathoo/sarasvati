package com.googlecode.sarasvati.editor;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;

public class NewGraphAction extends AbstractAction
{
  private static final long serialVersionUID = 1L;

  protected GraphEditor editor;

  public NewGraphAction (GraphEditor editor)
  {
    super( "New Process Definition" );
    this.editor = editor;

    putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_N, KeyEvent.CTRL_MASK ) );
    putValue( Action.MNEMONIC_KEY, KeyEvent.VK_N );
  }

  @Override
  public void actionPerformed (ActionEvent e)
  {
    editor.createNewProcessDefinition();
  }
}