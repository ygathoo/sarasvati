package com.googlecode.sarasvati.visual.graph.editor;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;

public class ExitAction extends AbstractAction
{
  private static final long serialVersionUID = 1L;

  protected ProcessDefinitionEditor editor;

  public ExitAction (ProcessDefinitionEditor editor)
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