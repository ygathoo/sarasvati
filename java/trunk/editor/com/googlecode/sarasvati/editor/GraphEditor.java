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

    Copyright 2008-2009 Paul Lorenz
*/
package com.googlecode.sarasvati.editor;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.KeyEventDispatcher;
import java.awt.KeyboardFocusManager;
import java.awt.Point;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.SwingUtilities;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.googlecode.sarasvati.editor.action.ArcSelectAction;
import com.googlecode.sarasvati.editor.action.ConnectAction;
import com.googlecode.sarasvati.editor.action.GraphMemberMoveAction;
import com.googlecode.sarasvati.editor.action.GraphMemberSelectAction;
import com.googlecode.sarasvati.editor.action.SceneAddExternalAction;
import com.googlecode.sarasvati.editor.action.SceneAddNodeAction;
import com.googlecode.sarasvati.editor.command.CommandStack;
import com.googlecode.sarasvati.editor.dialog.DialogFactory;
import com.googlecode.sarasvati.editor.menu.CopyAction;
import com.googlecode.sarasvati.editor.menu.CutAction;
import com.googlecode.sarasvati.editor.menu.DeleteAction;
import com.googlecode.sarasvati.editor.menu.ExitAction;
import com.googlecode.sarasvati.editor.menu.NewGraphAction;
import com.googlecode.sarasvati.editor.menu.OpenAction;
import com.googlecode.sarasvati.editor.menu.PasteAction;
import com.googlecode.sarasvati.editor.menu.PreferencesAction;
import com.googlecode.sarasvati.editor.menu.RedoAction;
import com.googlecode.sarasvati.editor.menu.SaveAction;
import com.googlecode.sarasvati.editor.menu.UndoAction;
import com.googlecode.sarasvati.editor.model.Clipboard;
import com.googlecode.sarasvati.editor.model.EditorGraph;
import com.googlecode.sarasvati.editor.model.EditorGraphFactory;
import com.googlecode.sarasvati.editor.model.EditorPreferences;
import com.googlecode.sarasvati.editor.model.EditorScene;
import com.googlecode.sarasvati.editor.model.Library;
import com.googlecode.sarasvati.load.SarasvatiLoadException;
import com.googlecode.sarasvati.load.definition.ProcessDefinition;
import com.googlecode.sarasvati.xml.XmlLoader;
import com.googlecode.sarasvati.xml.XmlProcessDefinition;

public class GraphEditor
{
  private static final String GRAPH_NAME_KEY = "graphName";
  private static GraphEditor INSTANCE;

  private enum SaveResult
  {
    SaveCanceled( true ),
    SaveFailed( true ),
    SaveNotWanted( false ),
    SaveSucceeded( false );

    private boolean abortExit;

    private SaveResult (final boolean abortExit)
    {
      this.abortExit = abortExit;
    }

    public boolean isAbortExit ()
    {
      return abortExit;
    }
  }

  public static GraphEditor getInstance ()
  {
    return INSTANCE;
  }

  protected XmlLoader   xmlLoader;
  protected JFrame      mainWindow;
  protected JPanel      mainPanel;
  protected JToolBar    toolBar;
  protected JTabbedPane tabPane;

  protected JToggleButton moveButton;
  protected JToggleButton editArcsButton;
  protected JToggleButton addNodesButton;
  protected JToggleButton addExternalsButton;

  protected SaveAction    saveAction;
  protected SaveAction    saveAsAction;

  protected DeleteAction  deleteAction;
  protected CutAction     cutAction;
  protected CopyAction    copyAction;
  protected PasteAction   pasteAction;
  protected UndoAction    undoAction;
  protected RedoAction    redoAction;

  protected EditorMode  mode;
  protected File        lastFile;

  public GraphEditor () throws SarasvatiLoadException
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

  public XmlLoader getXmlLoader ()
  {
    return xmlLoader;
  }

  public void setMode (EditorMode mode)
  {
    if ( this.mode == mode )
    {
      return;
    }

    GraphMemberMoveAction.setEnabled( false );
    SceneAddNodeAction.setEnabled( false );
    SceneAddExternalAction.setEnabled( false );
    ConnectAction.setEnabled( false );
    ArcSelectAction.setEnabled( false );
    GraphMemberSelectAction.setEnabled( false );

    moveButton.setSelected( false );
    addNodesButton.setSelected( false );
    addExternalsButton.setSelected( false );
    editArcsButton.setSelected( false );

    if ( mode == EditorMode.AddNode )
    {
      SceneAddNodeAction.setEnabled( true );
      addNodesButton.setSelected( true );
    }
    else if ( mode == EditorMode.AddExternal )
    {
      SceneAddExternalAction.setEnabled( true );
      addExternalsButton.setSelected( true );
    }
    else if ( mode == EditorMode.EditArcs )
    {
      ConnectAction.setEnabled( true );
      ArcSelectAction.setEnabled( true );
      editArcsButton.setSelected( true );

    }
    else if ( mode == EditorMode.Move )
    {
      GraphMemberMoveAction.setEnabled( true );
      GraphMemberSelectAction.setEnabled( true );
      ArcSelectAction.setEnabled( true );
      moveButton.setSelected( true );
    }

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
    mainWindow.setDefaultCloseOperation( JFrame.DO_NOTHING_ON_CLOSE );
    mainWindow.addWindowListener( new WindowAdapter()
    {
      @Override
      public void windowClosing (WindowEvent e)
      {
        exitRequested();
      }
    });

    mainWindow.setMinimumSize( new Dimension( 800, 600 ) );
    mainWindow.setJMenuBar( createMenu() );

    mainWindow.setVisible( true );

    DialogFactory.setFrame( mainWindow );

    toolBar = new JToolBar( "Tools" );
    toolBar.setFloatable( true );

    moveButton = new JToggleButton( "Move" );
    moveButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed (ActionEvent e)
      {
        setMode( EditorMode.Move );
      }
    });

    toolBar.add( moveButton );

    editArcsButton = new JToggleButton( "Edit Arcs" );
    editArcsButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed (ActionEvent e)
      {
        setMode( EditorMode.EditArcs );
      }
    });

    toolBar.add( editArcsButton );

    addNodesButton = new JToggleButton( "Add Nodes" );
    addNodesButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed (ActionEvent e)
      {
        setMode( EditorMode.AddNode );
      }
    });

    toolBar.add( addNodesButton );

    addExternalsButton = new JToggleButton( "Add Externals" );
    addExternalsButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed (ActionEvent e)
      {
        setMode( EditorMode.AddExternal );
      }
    });

    toolBar.add( addExternalsButton );

    JButton autoLayoutButton = new JButton( "Auto-Layout" );
    autoLayoutButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed (ActionEvent e)
      {
        getCurrentScene().autoLayout();
      }
    });

    toolBar.add( autoLayoutButton );

    tabPane = new JTabbedPane( JTabbedPane.TOP );
    tabPane.setTabLayoutPolicy( JTabbedPane.WRAP_TAB_LAYOUT );

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

    setupModeKeys();

    createNewProcessDefinition();
    tabSelectionChanged();
    setMode( EditorMode.Move );

    EditorPreferences prefs = EditorPreferences.getInstance();
    prefs.loadPreferences();

    if ( prefs.isFirstRun() )
    {
      DialogFactory.showInfo( "This is the first time the Sarasvati editor has been run. \n" +
                              "You may wish to configure a process definition library and custom node types" );

      JDialog dialog = DialogFactory.newPreferencesDialog();
      dialog.setVisible( true );
    }
  }

  protected JMenuBar createMenu ()
  {
    JMenuBar menuBar = new JMenuBar();

    JMenu fileMenu = new JMenu( "File" );
    fileMenu.setMnemonic( KeyEvent.VK_F );

    saveAction = new SaveAction( false );
    saveAsAction = new SaveAction( true );

    fileMenu.add( new JMenuItem( new NewGraphAction() ) );
    fileMenu.add( new JMenuItem( new OpenAction() ) );
    fileMenu.add( new JMenuItem( saveAsAction ) );
    fileMenu.add( new JMenuItem( saveAction ) );
    fileMenu.addSeparator();
    fileMenu.add( new JMenuItem( new ExitAction() ) );

    JMenu editMenu = new JMenu( "Edit" );
    editMenu.setMnemonic( KeyEvent.VK_E );

    deleteAction = new DeleteAction();
    cutAction    = new CutAction();
    copyAction   = new CopyAction();
    pasteAction  = new PasteAction();
    undoAction   = new UndoAction();
    redoAction   = new RedoAction();

    editMenu.add( new JMenuItem( deleteAction ) );
    editMenu.add( new JMenuItem( cutAction ) );
    editMenu.add( new JMenuItem( copyAction ) );
    editMenu.add( new JMenuItem( pasteAction ) );
    editMenu.addSeparator();
    editMenu.add( new JMenuItem( undoAction ) );
    editMenu.add( new JMenuItem( redoAction ) );
    editMenu.addSeparator();
    editMenu.add( new JMenuItem( new PreferencesAction() ) );

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
    scrollPane.putClientProperty( GRAPH_NAME_KEY, "Untitled" );
    addTab( "Untitled", scrollPane );
  }

  private void addTab (String name, JComponent component)
  {
    tabPane.addTab( name, component );
    tabPane.setTabComponentAt( tabPane.getTabCount() - 1, new TabComponent( tabPane, name ) );
    tabPane.setSelectedComponent( component );
  }

  public void openProcessDefinition (File processDefinitionFile)
  {
    if ( getCurrentScene() != null &&
         getCurrentScene().getGraph().getFile() == null &&
         CommandStack.getCurrent().isEmpty() )
    {
      closeCurrentTab();
    }

    try
    {
      ProcessDefinition xmlProcDef = xmlLoader.translate( processDefinitionFile );
      EditorGraph graph = EditorGraphFactory.loadFromXml( xmlProcDef );
      graph.setFile( processDefinitionFile );
      EditorScene scene = new EditorScene( graph );

      JScrollPane scrollPane = new JScrollPane();
      scrollPane.setViewportView( scene.createView() );
      addTab( graph.getName(), scrollPane );

      scrollPane.putClientProperty( "scene", scene );
      scrollPane.putClientProperty( GRAPH_NAME_KEY, graph.getName() );
      tabSelectionChanged();
    }
    catch (Exception e)
    {
      e.printStackTrace();
      JOptionPane.showMessageDialog( mainWindow, e.getMessage(), "Load Error", JOptionPane.ERROR_MESSAGE );
    }
  }

  public void setupModeKeys ()
  {
    final EditorKeyListener keyListener = new EditorKeyListener();
    final KeyboardFocusManager keyboardFocusManager = KeyboardFocusManager.getCurrentKeyboardFocusManager();
    keyboardFocusManager.addKeyEventDispatcher( new KeyEventDispatcher()
    {
      @Override
      public boolean dispatchKeyEvent (final KeyEvent event)
      {
        Window window = keyboardFocusManager.getActiveWindow();

        if ( window == mainWindow )
        {
          if ( event.getID() == KeyEvent.KEY_PRESSED )
          {
            keyListener.keyPressed( event );
          }
          else if ( event.getID() == KeyEvent.KEY_RELEASED )
          {
            keyListener.keyReleased( event );
          }
        }
        return false;
      }
    });
  }

  public SaveResult saveRequested (final boolean isSaveAs)
  {
    EditorScene scene = getCurrentScene();

    if ( scene == null )
    {
      return null;
    }

    EditorGraph graph = scene.getGraph();

    List<String> errors = graph.validateGraph();

    if ( !errors.isEmpty() )
    {
      StringBuilder buf = new StringBuilder ();
      for ( String error : errors )
      {
        buf.append( error );
        buf.append( "\n" );
      }

      JOptionPane.showMessageDialog( GraphEditor.getInstance().getMainWindow(),
                                     buf.toString(),
                                     "Invalid Process Definition",
                                     JOptionPane.ERROR_MESSAGE );
      return SaveResult.SaveFailed;
    }

    if ( isSaveAs || scene.getGraph().getFile() == null )
    {
      File basePath = Library.getInstance().getBasePath();

      if ( basePath == null )
      {
        basePath = getLastFile();
      }

      JFileChooser fileChooser = new JFileChooser();
      fileChooser.setCurrentDirectory( basePath );

      int retVal = fileChooser.showSaveDialog( mainWindow );

      if ( retVal == JFileChooser.APPROVE_OPTION )
      {
        setLastFile( fileChooser.getSelectedFile() );
        return saveProcessDefinition( graph, fileChooser.getSelectedFile() );
      }
      else
      {
        return SaveResult.SaveCanceled;
      }
    }
    else
    {
      return saveProcessDefinition( graph, graph.getFile() );
    }
  }

  public SaveResult saveProcessDefinition (final EditorGraph graph,
                                           final File outputFile)
  {
    File saveFile = null;

    String name = outputFile.getName();

    int firstDot = name.indexOf( '.' );

    if ( firstDot > 0 )
    {
      name = name.substring( 0, firstDot );
      saveFile = outputFile;
    }
    else
    {
      saveFile = new File( outputFile.getParentFile(), name + ".wf.xml" );
    }

    graph.setName( name );
    JComponent c = (JComponent)tabPane.getSelectedComponent();
    c.putClientProperty( GRAPH_NAME_KEY, name );
    updateTabTitle( tabPane.getSelectedIndex(), name );

    try
    {
      XmlProcessDefinition xmlProcDef = EditorGraphFactory.exportToXml( graph );
      xmlLoader.saveProcessDefinition( xmlProcDef, saveFile );
      graph.setFile( outputFile );
      CommandStack.markSaved();

      JOptionPane.showMessageDialog( mainWindow,
                                     "Process definition successfully saved to: '" + saveFile.getPath() + "'",
                                     "Save", JOptionPane.INFORMATION_MESSAGE );

      return SaveResult.SaveSucceeded;
    }
    catch ( Exception e )
    {
      e.printStackTrace();
      JOptionPane.showMessageDialog( mainWindow, e.getMessage(), "Save Error", JOptionPane.ERROR_MESSAGE );
      return SaveResult.SaveFailed;
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
    updateMenu();
  }

  public void updateMenu ()
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

    if ( currentCommandStack != null )
    {
      boolean isUnSaved = !currentCommandStack.isSaved();
      saveAction.setEnabled( isUnSaved );
      saveAsAction.setEnabled( true );

      JComponent c = (JComponent)tabPane.getSelectedComponent();
      String title = (String)c.getClientProperty( GRAPH_NAME_KEY );
      if ( isUnSaved )
      {
        updateTabTitle( tabPane.getSelectedIndex(), "*" + title );
      }
      else
      {
        updateTabTitle( tabPane.getSelectedIndex(), title );
      }
    }
    else
    {
      saveAction.setEnabled( false );
      saveAsAction.setEnabled( false );
    }

    updateCutCopyPaste();
  }

  public void updateCutCopyPaste ()
  {
    EditorScene scene = getCurrentScene();
    if (scene == null )
    {
      deleteAction.setEnabled( false );
      cutAction.setEnabled( false );
      copyAction.setEnabled( false );
      pasteAction.setEnabled( false );
    }
    else
    {
      updateCutCopyPaste( scene );
    }
  }

  public void updateCutCopyPaste (final EditorScene scene)
  {
    boolean hasSelection = !scene.getSelectedObjects().isEmpty();
    deleteAction.setEnabled( hasSelection );
    cutAction.setEnabled( hasSelection );
    copyAction.setEnabled( hasSelection );
    pasteAction.setEnabled( Clipboard.getInstance().isClipboardPasteable() );
  }

  private void updateTabTitle (final int index, final String label)
  {
    tabPane.setTitleAt( index, label );
    TabComponent tabComp = (TabComponent) tabPane.getTabComponentAt( tabPane.getSelectedIndex() );
    if ( tabComp != null )
    {
      tabComp.setLabelText( label );
    }
  }

  public void exitRequested ()
  {
    if ( tabPane.getSelectedIndex() > 0 )
    {
      if ( closeCurrentTab().isAbortExit() )
      {
        return;
      }
    }

    while ( tabPane.getTabCount() > 0 )
    {
      tabPane.setSelectedIndex( 0 );
      tabSelectionChanged();
      if ( closeCurrentTab().isAbortExit() )
      {
        return;
      }
    }

    System.exit( 0 );
  }

  public void closeTab (int index)
  {
    Component previous = tabPane.getSelectedComponent();
    boolean returnToPrev = tabPane.getSelectedIndex() != index;

    if ( returnToPrev )
    {
      tabPane.setSelectedIndex( index );
    }

    SaveResult result = closeCurrentTab();

    if ( returnToPrev && !result.isAbortExit() )
    {
      tabPane.setSelectedComponent( previous );
    }
  }

  public SaveResult closeCurrentTab ()
  {
    if ( !CommandStack.getCurrent().isSaved() )
    {
      JComponent c = (JComponent)tabPane.getSelectedComponent();
      String title = (String)c.getClientProperty( GRAPH_NAME_KEY );
      int result =
        JOptionPane.showConfirmDialog( mainWindow, "Process definition '" + title + "' has unsaved changes. " +
                                                    "Do you wish to save your work before exiting?" );
      if ( JOptionPane.YES_OPTION == result )
      {
        SaveResult saveResult = saveRequested( false );
        if ( saveResult == SaveResult.SaveSucceeded )
        {
          tabPane.remove( tabPane.getSelectedIndex() );
        }
        return saveResult;
      }
      else if ( JOptionPane.NO_OPTION == result )
      {
        tabPane.remove( tabPane.getSelectedIndex() );
        return SaveResult.SaveNotWanted;
      }

      return SaveResult.SaveCanceled;
    }

    tabPane.remove( tabPane.getSelectedIndex() );
    return SaveResult.SaveNotWanted;
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

  public void editPaste ()
  {
    EditorScene scene = getCurrentScene();
    if ( scene != null )
    {
      scene.editPaste( new Point( 0, 0 ) );
    }
  }

  public void editCopy ()
  {
    EditorScene scene = getCurrentScene();
    if ( scene != null )
    {
      scene.editCopy();
    }
  }

  public void editCut ()
  {
    EditorScene scene = getCurrentScene();
    if ( scene != null )
    {
      scene.editCut();
    }
  }

  public void editDelete ()
  {
    EditorScene scene = getCurrentScene();
    if ( scene != null )
    {
      scene.editDelete();
    }
  }

  public void nodeTypesChanged ()
  {
    for ( Component component : tabPane.getComponents() )
    {
      if ( component instanceof JComponent )
      {
        JComponent jComponent = (JComponent)component;
        EditorScene scene = (EditorScene)jComponent.getClientProperty( "scene" );
        if ( scene != null )
        {
          scene.nodeTypesUpdated();
        }
      }
    }
  }
}