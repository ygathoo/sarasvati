package com.googlecode.sarasvati.editor;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;

public class ExitAction extends AbstractAction
{
  private static final long serialVersionUID = 1L;

  protected GraphEditor editor;

  public ExitAction (GraphEditor editor)
  {
    super( "Exit" );
    this.editor = editor;

    putValue( Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke( KeyEvent.VK_Q, KeyEvent.CTRL_MASK ) );
    putValue( Action.MNEMONIC_KEY, KeyEvent.VK_X );
  }

  @Override
  public void actionPerformed (ActionEvent e)
  {
    System.exit( 0 );
  }
}