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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.xml.bind.JAXBException;

import com.googlecode.sarasvati.editor.command.CommandStack;
import com.googlecode.sarasvati.editor.menu.ExitAction;
import com.googlecode.sarasvati.editor.menu.NewGraphAction;
import com.googlecode.sarasvati.editor.menu.OpenAction;
import com.googlecode.sarasvati.editor.menu.RedoAction;
import com.googlecode.sarasvati.editor.menu.SaveAction;
import com.googlecode.sarasvati.editor.menu.UndoAction;
import com.googlecode.sarasvati.editor.model.EditorGraph;
import com.googlecode.sarasvati.editor.model.EditorGraphFactory;
import com.googlecode.sarasvati.editor.model.EditorScene;
import com.googlecode.sarasvati.editor.model.MoveTrackAction;
import com.googlecode.sarasvati.editor.toolbar.AddNodeModeAction;
import com.googlecode.sarasvati.editor.toolbar.MoveModeAction;
import com.googlecode.sarasvati.load.LoadException;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class GraphEditor
{
  private static GraphEditor INSTANCE;

  public static GraphEditor getInstance ()
  {
    return INSTANCE;
  }

  protected XmlLoader   xmlLoader;
  protected JFrame      mainWindow;
  protected JPanel      mainPanel;
  protected JToolBar    toolBar;
  protected JTabbedPane tabPane;

  protected SaveAction saveAction;
  protected UndoAction undoAction;
  protected RedoAction redoAction;

  protected EditorMode  mode;
  protected File        lastFile;

  public GraphEditor () throws JAXBException, LoadException
  {
    INSTANCE = this;
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

  public File getLastFile ()
  {
    return lastFile;
  }

  public void setLastFile (File lastFile)
  {
    this.lastFile = lastFile;
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

    tabPane.addChangeListener( new ChangeListener()
    {
      @Override
      public void stateChanged (final ChangeEvent e)
      {
        tabSelectionChanged();
      }
    });

    mainPanel = new JPanel ();
    mainPanel.setLayout( new BorderLayout() );

    mainPanel.add( toolBar, BorderLayout.PAGE_START );
    mainPanel.add( tabPane, BorderLayout.CENTER );

    mainWindow.setContentPane( mainPanel );

    createNewProcessDefinition();
    modeAddNode();
  }

  protected JMenuBar createMenu ()
  {
    JMenuBar menuBar = new JMenuBar();

    JMenu fileMenu = new JMenu( "File" );
    fileMenu.setMnemonic( KeyEvent.VK_F );

    fileMenu.add( new JMenuItem( new NewGraphAction() ) );
    fileMenu.add( new JMenuItem( new OpenAction() ) );
    fileMenu.add( new JMenuItem( new SaveAction( true ) ) );
    fileMenu.add( new JMenuItem( new SaveAction( false ) ) );
    fileMenu.add( new JMenuItem( new ExitAction() ) );

    JMenu editMenu = new JMenu( "Edit" );
    fileMenu.setMnemonic( KeyEvent.VK_E );

    undoAction = new UndoAction();
    redoAction = new RedoAction();

    editMenu.add( new JMenuItem( undoAction ) );
    editMenu.add( new JMenuItem( redoAction ) );

    menuBar.add( fileMenu );
    menuBar.add( editMenu );

    return menuBar;
  }

  public void createNewProcessDefinition ()
  {
    final JScrollPane scrollPane = new JScrollPane();

    final EditorScene scene = new EditorScene( new EditorGraph() );
    scrollPane.setViewportView( scene.createView() );
    scrollPane.putClientProperty( "scene", scene );

    tabPane.addTab( "Untitled", scrollPane );
    tabPane.setSelectedComponent( scrollPane );
    tabSelectionChanged();
  }

  public void openProcessDefinition (File processDefinitionFile)
  {
    try
    {
      XmlProcessDefinition xmlProcDef = xmlLoader.loadProcessDefinition( processDefinitionFile );
      EditorGraph graph = EditorGraphFactory.loadFromXml( xmlProcDef );
      EditorScene scene = new EditorScene( graph );

      JScrollPane scrollPane = new JScrollPane();
      scrollPane.setViewportView( scene.createView() );
      tabPane.addTab( graph.getName(), scrollPane );
      tabPane.setSelectedComponent( scrollPane );

      scrollPane.putClientProperty( "scene", scene );
      tabSelectionChanged();
    }
    catch (Exception e)
    {
      JOptionPane.showMessageDialog( mainWindow, e.getMessage(), "Load Error", JOptionPane.ERROR_MESSAGE );
    }
  }

  public void saveProcessDefinition (EditorGraph graph, File outputFile)
  {
    String name = outputFile.getName();
    int firstDot = name.indexOf(  '.' );

    if ( firstDot > 0 )
    {
      name = name.substring( 0, firstDot );
    }

    graph.setName( name );
    tabPane.setTitleAt( tabPane.getSelectedIndex(), name );

    try
    {
      XmlProcessDefinition xmlProcDef = EditorGraphFactory.exportToXml( graph );
      xmlLoader.saveProcessDefinition( xmlProcDef, outputFile );
      graph.setFile( outputFile );
    }
    catch ( Exception e )
    {
      JOptionPane.showMessageDialog( mainWindow, e.getMessage(), "Save Error", JOptionPane.ERROR_MESSAGE );
    }
  }

  public void modeMove ()
  {
    SceneAddNodeAction.setEnabled( false );
    MoveTrackAction.setEnabled(  true );
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
    MoveTrackAction.setEnabled( false );

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

  public void tabSelectionChanged ()
  {
    EditorScene current = getCurrentScene();
    CommandStack.setCurrent( current == null ? null : current.getCommandStack() );
    updateUndoRedo();
  }

  public void updateUndoRedo ()
  {
    CommandStack currentCommandStack = CommandStack.getCurrent();

    if ( currentCommandStack != null )
    {
      undoAction.setEnabled( currentCommandStack.canUndo() );
      redoAction.setEnabled( currentCommandStack.canRedo() );
    }
    else
    {
      undoAction.setEnabled( false );
      redoAction.setEnabled( false );
    }

    if ( undoAction.isEnabled() )
    {
      undoAction.setName( "Undo: " + currentCommandStack.getUndoName() );
    }
    else
    {
      undoAction.setName( "Undo" );
    }

    if ( redoAction.isEnabled() )
    {
      redoAction.setName( "Redo: " + currentCommandStack.getRedoName() );
    }
    else
    {
      redoAction.setName( "Redo" );
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
