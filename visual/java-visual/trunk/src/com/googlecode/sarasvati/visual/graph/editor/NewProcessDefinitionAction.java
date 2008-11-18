package com.googlecode.sarasvati.visual.graph.editor;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.KeyStroke;

public class NewProcessDefinitionAction extends AbstractAction
{
  private static final long serialVersionUID = 1L;

  protected ProcessDefinitionEditor editor;

  public NewProcessDefinitionAction (ProcessDefinitionEditor editor)
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