package com.googlecode.sarasvati.editor;

import java.awt.event.KeyEvent;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;

public class GraphEditor
{
  protected JFrame      mainWindow;
  protected JTabbedPane tabPane;

  public JFrame getMainWindow ()
  {
    return mainWindow;
  }

  protected void setup ()
  {
    mainWindow = new JFrame( "Sarasvati Graph Editor" );
    mainWindow.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    mainWindow.setSize( 800, 600 );
    mainWindow.setJMenuBar( createMenu() );
    mainWindow.setVisible( true );

    tabPane = new JTabbedPane( JTabbedPane.TOP );
    tabPane.setTabLayoutPolicy( JTabbedPane.SCROLL_TAB_LAYOUT );

    mainWindow.setContentPane( tabPane );
  }

  protected JMenuBar createMenu ()
  {
    JMenuBar menuBar = new JMenuBar();

    JMenu fileMenu = new JMenu( "File" );
    fileMenu.setMnemonic( KeyEvent.VK_F );

    fileMenu.add( new JMenuItem( new NewGraphAction( this ) ) );
    fileMenu.add( new JMenuItem( new OpenAction( this ) ) );
    fileMenu.add( new JMenuItem( new ExitAction( this ) ) );

    menuBar.add( fileMenu );
    return menuBar;
  }

  public void createNewProcessDefinition ()
  {
    JScrollPane scrollPane = new JScrollPane();
    tabPane.addTab( "Untitled", scrollPane );
  }

  public static void main( String[] args )
  {
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override public void run()
      {
        try
        {
          UIManager.setLookAndFeel("com.sun.java.swing.plaf.nimbus.NimbusLookAndFeel");
        }
        catch ( Exception e )
        {
          System.out.println( "Nimbus not supported on your JRE" );
        }

        new GraphEditor().setup();
      }
    });
  }
}
