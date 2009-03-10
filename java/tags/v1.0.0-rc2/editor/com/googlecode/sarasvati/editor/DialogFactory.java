package com.googlecode.sarasvati.editor;

import javax.swing.JDialog;
import javax.swing.JFrame;

import com.googlecode.sarasvati.editor.model.EditorNode;

public class DialogFactory
{
  protected static JFrame frame = null;

  public static void setFrame (JFrame frame)
  {
    DialogFactory.frame = frame;
  }

  public static JDialog newPropertiesDialog (EditorNode node)
  {
    return new NodePropertiesDialog( frame, node );
  }
}
