package com.googlecode.sarasvati.visual.graph;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

public class GraphEditor extends JFrame
{
  private static final long serialVersionUID = 1L;

  protected void setup ()
  {
    setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    setupMenu();
    setVisible( true );
  }

  protected void setupMenu ()
  {
    JMenuBar menuBar = new JMenuBar();

    JMenu fileMenu = new JMenu( "File" );
    fileMenu.setMnemonic( KeyEvent.VK_F );
    //fileMenu.setAccelerator( KeyStroke.getKeyStroke( KeyEvent.VK_F, ActionEvent.ALT_MASK ) );

    JMenuItem openItem = new JMenuItem( "Open", KeyEvent.VK_O );
    openItem.setAction( new AbstractAction()
    {
      private static final long serialVersionUID = 1L;

      @Override
      public void actionPerformed (ActionEvent e)
      {
        JOptionPane.showMessageDialog( GraphEditor.this, "Open" );
      }
    });

    fileMenu.add( openItem );
    menuBar.add( fileMenu );

    setJMenuBar( menuBar );
  }

  public static void main( String[] args )
  {
    new GraphEditor().setup();
  }
}
