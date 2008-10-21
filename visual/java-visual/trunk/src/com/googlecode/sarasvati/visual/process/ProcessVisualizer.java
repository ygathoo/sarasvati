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
package com.googlecode.sarasvati.visual.process;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;
import java.util.List;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.hibernate.Session;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.example.db.TestSetup;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.hib.HibGraphProcess;
import com.googlecode.sarasvati.visual.NodeDrawConfig;

public class ProcessVisualizer
{
  protected static HibGraphProcess   currentProcess = null;
  protected static SarasvatiProcessScene scene = new SarasvatiProcessScene();

  final JScrollPane scrollPane = new JScrollPane();

  public static void main (String[] args) throws Exception
  {
    new ProcessVisualizer().run();
  }

  public void init () throws Exception
  {
    TestSetup.init();
  }

  public Session getSession  ()
  {
    return TestSetup.openSession();
  }

  @SuppressWarnings("unchecked")
  public void run () throws Exception
  {
    init ();

    final Session session = getSession();
    HibEngine engine = new HibEngine( session );

    List<GraphProcess> process = engine.getSession().createQuery( "from HibGraphProcess order by graph, createDate" ).list();

    JFrame frame = new JFrame( "Workflow Visualizer" );
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    frame.setMinimumSize(  new Dimension( 800, 600 ) );

    JSplitPane splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT );
    frame.getContentPane().add( splitPane );

    DefaultListModel listModel = new DefaultListModel();
    for ( GraphProcess p : process )
    {
      listModel.addElement( p );
    }

    ListCellRenderer cellRenderer = new DefaultListCellRenderer()
    {
      private static final long serialVersionUID = 1L;

      @Override
      public Component getListCellRendererComponent( JList list, Object value,
                                                     int index, boolean isSelected,
                                                     boolean cellHasFocus )
      {
        super.getListCellRendererComponent( list, value, index, isSelected, cellHasFocus );

        HibGraphProcess p = (HibGraphProcess)value;

        setText( p.getGraph().getName() + "-" + p.getId() + "  " );
        return this;
      }
    };

    final JList graphList = new JList( listModel );
    graphList.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
    graphList.setCellRenderer( cellRenderer );

    JScrollPane listScrollPane = new JScrollPane(graphList );
    listScrollPane.setHorizontalScrollBarPolicy( ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED );
    listScrollPane.setVerticalScrollBarPolicy( ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS );

    splitPane.add( listScrollPane );

    scrollPane.setViewportView( scene.createView() );
    scrollPane.setHorizontalScrollBarPolicy( ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED );
    scrollPane.setVerticalScrollBarPolicy( ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED );

    splitPane.add( scrollPane );
    scrollPane.setBackground( Color.white );

    graphList.addListSelectionListener( new ListSelectionListener()
    {
      @Override
      public void valueChanged( ListSelectionEvent e )
      {
        if ( e.getValueIsAdjusting() )
        {
          return;
        }

        final HibGraphProcess graphProcess = (HibGraphProcess)graphList.getSelectedValue();

        if ( graphProcess != null && graphProcess.equals( currentProcess ) )
        {
          return;
        }

        if ( graphProcess != null )
        {
          session.refresh( graphProcess );
        }

        setProcess( graphProcess );
      }
    } );

    frame.setVisible( true );

    new Thread()
    {
      @Override public void run ()
      {
        while ( true )
        {
          try
          {
            synchronized( this )
            {
              wait( 1000 );
            }

            SwingUtilities.invokeLater( new Runnable()
            {
              @Override public void run()
              {
                if ( currentProcess != null )
                {
                  Integer currentVersion = currentProcess.getVersion();
                  session.clear();
                  session.refresh( currentProcess );

                  if ( currentVersion != null &&
                       currentProcess.getVersion() != null &&
                       currentVersion.intValue() !=  currentProcess.getVersion().intValue() )
                  {
                    setProcess( currentProcess );
                  }
                }
              }
            });
          }
          catch( InterruptedException ie )
          {
            return;
          }
        }
      }
    }.run();
  }

  public synchronized void setProcess (final HibGraphProcess graphProcess)
  {
    currentProcess = graphProcess;
    scene = new SarasvatiProcessScene();

    if ( graphProcess == null )
    {
      scrollPane.setViewportView( scene.createView() );
      scene.repaint();
      return;
    }

    ProcessTree pt = new ProcessTree( currentProcess );
    Iterable<ProcessTreeNode> nodes = pt.getProcessTreeNodes();

    for ( ProcessTreeNode node : nodes )
    {
      scene.addNode( node );
      Widget widget = scene.findWidget( node );
      widget.setPreferredLocation( new Point( node.getOriginX(), node.getOriginY() ) );
    }

    for ( ProcessTreeNode node : nodes )
    {
      for ( ProcessTreeArc ptArc : node.getChildren() )
      {
        scene.addEdge( ptArc );
        scene.setEdgeSource( ptArc, ptArc.getParent() );
        scene.setEdgeTarget( ptArc, ptArc.getChild() );

        ConnectionWidget w = (ConnectionWidget)scene.findWidget( ptArc );

        ArcToken token =  ptArc.getToken();
        if ( token != null )
        {
          w.setStroke( new BasicStroke( 3 ) );
          if ( token.isComplete() )
          {
            w.setLineColor( NodeDrawConfig.NODE_BG_COMPLETED );
          }
          else
          {
            w.setLineColor( NodeDrawConfig.NODE_BG_ACTIVE );
          }
        }
      }
    }

    scene.revalidate();
    scrollPane.setViewportView( scene.createView() );
    scrollPane.repaint();
  }
}