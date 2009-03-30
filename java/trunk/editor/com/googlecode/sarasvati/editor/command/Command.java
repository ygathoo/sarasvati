package com.googlecode.sarasvati.editor.command;

public interface Command
{
  void performAction ();

  void undoAction ();
}
