package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.codemonk.wf.db.Graph;
import org.codemonk.wf.db.HibernateEngine;
import org.codemonk.wf.test.TestSetup;
import org.hibernate.Session;

public class Visualizer
{
  @SuppressWarnings("serial")
  public static void main( String[] args ) throws Exception
  {
    TestSetup.init();

    Session session = TestSetup.openSession();
    HibernateEngine engine = new HibernateEngine( session );

    JFrame frame = new JFrame( "Workflow Visualizer" );
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    frame.setMinimumSize(  new Dimension( 800, 600 ) );

    JSplitPane splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT );
    frame.getContentPane().add( splitPane );

    DefaultListModel listModel = new DefaultListModel();
    for ( Graph g : engine.getGraphs() )
    {
      listModel.addElement( g );
    }

    ListCellRenderer cellRenderer = new DefaultListCellRenderer()
    {
      @Override
      public Component getListCellRendererComponent( JList list, Object value,
                                                     int index, boolean isSelected,
                                                     boolean cellHasFocus )
      {
        super.getListCellRendererComponent( list, value, index, isSelected, cellHasFocus );
        setText( ((Graph)value).getName() );
        return this;
      }
    };

    final JList graphList = new JList( listModel );
    graphList.setSelectionMode( ListSelectionModel.SINGLE_SELECTION );
    graphList.setCellRenderer( cellRenderer );

    JScrollPane listScrollPane = new JScrollPane(graphList );
    listScrollPane.setHorizontalScrollBarPolicy( JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
    listScrollPane.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_ALWAYS );

    splitPane.add( listScrollPane );

    final GraphDrawing graphDrawing = new GraphDrawing();

    JScrollPane scrollPane = new JScrollPane( graphDrawing );
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

        final Graph g = (Graph)graphList.getSelectedValue();

        if ( ( g == null && graphDrawing.getGraph() == null ) ||
             (g != null && g.equals( graphDrawing.getGraph() ) ) )
        {
          return;
        }

        SwingUtilities.invokeLater( new Runnable()
        {
          @Override
          public void run()
          {
            graphDrawing.setGraph( g );
            graphDrawing.repaint();
          }
        });
      }
    } );

    frame.setVisible( true );
  }
}