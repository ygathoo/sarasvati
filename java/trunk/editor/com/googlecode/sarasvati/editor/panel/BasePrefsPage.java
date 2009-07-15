package com.googlecode.sarasvati.editor.panel;

import javax.swing.JPanel;

public class BasePrefsPage extends JPanel
{
  private static final long serialVersionUID = 1L;

  private boolean setupDone = false;

  public void setup ()
  {
    // does nothing by default
  }

  public void displayPage ()
  {
    // does nothing by default
  }

  public boolean isSetupDone ()
  {
    return setupDone;
  }

  public void setSetupDone (boolean setupDone)
  {
    this.setupDone = setupDone;
  }
}
