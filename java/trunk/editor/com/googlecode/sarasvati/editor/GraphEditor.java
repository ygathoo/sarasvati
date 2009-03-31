/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
package com.googlecode.sarasvati.editor;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.KeyEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.xml.bind.JAXBException;

import com.googlecode.sarasvati.editor.menu.ExitAction;
import com.googlecode.sarasvati.editor.menu.NewGraphAction;
import com.googlecode.sarasvati.editor.menu.OpenAction;
import com.googlecode.sarasvati.editor.menu.RedoAction;
import com.googlecode.sarasvati.editor.menu.UndoAction;
import com.googlecode.sarasvati.editor.model.EditorGraph;
import com.googlecode.sarasvati.editor.model.EditorGraphFactory;
import com.googlecode.sarasvati.editor.model.EditorScene;
import com.googlecode.sarasvati.editor.toolbar.AddNodeModeAction;
import com.googlecode.sarasvati.editor.toolbar.MoveModeAction;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class GraphEditor
{
  protected XmlLoader   xmlLoader;
  protected JFrame      mainWindow;
  protected JPanel      mainPanel;
  protected JToolBar    toolBar;
  protected JTabbedPane tabPane;

  protected EditorMode  mode;

  public GraphEditor () throws JAXBException, LoadException
  {
    xmlLoader = new XmlLoader();
  }

  public JFrame getMainWindow ()
  {
    return mainWindow;
  }

  public EditorMode getMode ()
  {
    return mode;
  }

  public void setMode (EditorMode mode)
  {
    this.mode = mode;
  }

  protected void setup ()
  {
    mainWindow = new JFrame( "Sarasvati Graph Editor" );
    mainWindow.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    mainWindow.setMinimumSize( new Dimension( 800, 600 ) );
    mainWindow.setJMenuBar( createMenu() );
    mainWindow.setVisible( true );

    DialogFactory.setFrame( mainWindow );

    toolBar = new JToolBar( "Tools" );
    toolBar.setFloatable( true );

    JButton moveButton = new JButton( "Move" );
    moveButton.setAction( new MoveModeAction( this ) );

    toolBar.add( moveButton );
    toolBar.add( new JButton( "Edit Arcs" ) );

    JButton addNodeButton = new JButton( "Add Nodes" );
    addNodeButton.setAction( new AddNodeModeAction( this ) );
    toolBar.add( addNodeButton );
    toolBar.add( new JButton( "Add External" ) );
    toolBar.add( new JButton( "Add Tasks" ) );

    tabPane = new JTabbedPane( JTabbedPane.TOP );
    tabPane.setTabLayoutPolicy( JTabbedPane.SCROLL_TAB_LAYOUT );

    mainPanel = new JPanel ();
    mainPanel.setLayout( new BorderLayout() );

    mainPanel.add( toolBar, BorderLayout.PAGE_START );
    mainPanel.add( tabPane, BorderLayout.CENTER );

    mainWindow.setContentPane( mainPanel );

    createNewProcessDefinition();
  }

  protected JMenuBar createMenu ()
  {
    JMenuBar menuBar = new JMenuBar();

    JMenu fileMenu = new JMenu( "File" );
    fileMenu.setMnemonic( KeyEvent.VK_F );

    fileMenu.add( new JMenuItem( new NewGraphAction( this ) ) );
    fileMenu.add( new JMenuItem( new OpenAction( this ) ) );
    fileMenu.add( new JMenuItem( new ExitAction( this ) ) );

    JMenu editMenu = new JMenu( "Edit" );
    fileMenu.setMnemonic( KeyEvent.VK_E );

    editMenu.add( new JMenuItem( new UndoAction( this ) ) );
    editMenu.add( new JMenuItem( new RedoAction( this ) ) );

    menuBar.add( fileMenu );
    menuBar.add( editMenu );

    return menuBar;
  }

  public void createNewProcessDefinition ()
  {
    final JScrollPane scrollPane = new JScrollPane();
    tabPane.addTab( "Untitled", scrollPane );

    final EditorScene scene = new EditorScene( this, new EditorGraph() );
    scrollPane.setViewportView( scene.createView() );
    scrollPane.putClientProperty( "scene", scene );
  }

  public void openProcessDefinition (File processDefinitionFile)
  {
    try
    {
      XmlProcessDefinition xmlProcDef = xmlLoader.loadProcessDefinition( processDefinitionFile );
      EditorGraph graph = EditorGraphFactory.loadFromXml( xmlProcDef );
      EditorScene scene = new EditorScene( this, graph );

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

  public void modeMove ()
  {
    SceneAddNodeAction.setEnabled( false );
    if ( this.mode != EditorMode.Move )
    {
      this.mode = EditorMode.Move;
      EditorScene scene = getCurrentScene();
      if ( scene != null )
      {
        scene.modeMove();
      }
    }
  }

  public void modeAddNode ()
  {
    SceneAddNodeAction.setEnabled( true );
    if ( this.mode != EditorMode.AddNode )
    {
      this.mode = EditorMode.AddNode;
      EditorScene scene = getCurrentScene();
      if ( scene != null )
      {
        scene.modeAddNode();
      }
    }
  }

  public EditorScene getCurrentScene ()
  {
    JComponent c = (JComponent)tabPane.getSelectedComponent();
    return c != null ? (EditorScene)c.getClientProperty( "scene" ) : null;
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
