package com.googlecode.sarasvati.editor;

import java.awt.Dimension;
import java.awt.event.KeyEvent;
import java.io.File;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.SwingUtilities;
import javax.xml.bind.JAXBException;

import com.googlecode.sarasvati.editor.model.EditorArc;
import com.googlecode.sarasvati.editor.model.EditorGraph;
import com.googlecode.sarasvati.editor.model.EditorGraphFactory;
import com.googlecode.sarasvati.editor.model.EditorGraphMember;
import com.googlecode.sarasvati.editor.model.EditorScene;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class GraphEditor
{
  protected XmlLoader   xmlLoader;
  protected JFrame      mainWindow;
  protected JTabbedPane tabPane;

  public GraphEditor () throws JAXBException, LoadException
  {
    xmlLoader = new XmlLoader();
  }

  public JFrame getMainWindow ()
  {
    return mainWindow;
  }

  protected void setup ()
  {
    mainWindow = new JFrame( "Sarasvati Graph Editor" );
    mainWindow.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    mainWindow.setMinimumSize( new Dimension( 800, 600 ) );
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

  public void openProcessDefinition (File processDefinitionFile)
  {
    try
    {
      XmlProcessDefinition xmlProcDef = xmlLoader.loadProcessDefinition(  processDefinitionFile );
      EditorGraph graph = EditorGraphFactory.loadFromXml( xmlProcDef );
      EditorScene scene = new EditorScene();

      for ( EditorGraphMember member : graph.getMembers().values() )
      {
        scene.addNode( member );
      }

      for ( EditorArc arc : graph.getArcs() )
      {
        scene.addEdge( arc );
        scene.setEdgeSource(  arc, arc.getStart() );
        scene.setEdgeTarget( arc, arc.getEnd() );
      }

      JScrollPane scrollPane = new JScrollPane();
      scrollPane.setViewportView( scene.createView() );
      tabPane.addTab( graph.getName(), scrollPane );
      tabPane.setSelectedComponent( scrollPane );
    }
    catch (Exception e)
    {
      JOptionPane.showMessageDialog( mainWindow, e.getMessage(), "Load Error", JOptionPane.ERROR_MESSAGE );
    }
  }

  public static void main( String[] args ) throws Exception
  {
    final GraphEditor graphEditor = new GraphEditor();
    SwingUtilities.invokeLater( new Runnable()
    {
      @Override public void run()
      {
        graphEditor.setup();
      }
    });
  }
}
