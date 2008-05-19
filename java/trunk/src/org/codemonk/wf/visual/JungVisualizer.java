package org.codemonk.wf.visual;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.codemonk.wf.db.Arc;
import org.codemonk.wf.db.Graph;
import org.codemonk.wf.db.HibernateEngine;
import org.codemonk.wf.db.NodeRef;
import org.codemonk.wf.test.TestSetup;
import org.hibernate.Session;

import edu.uci.ics.jung.algorithms.layout.KKLayout;
import edu.uci.ics.jung.algorithms.layout.SpringLayout2;
import edu.uci.ics.jung.algorithms.layout.TreeLayout;
import edu.uci.ics.jung.graph.DirectedSparseGraph;
import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.visualization.BasicVisualizationServer;

public class JungVisualizer
{
  protected static Graph currentGraph = null;

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

    //TreeLayout<NodeRef, Arc> layout = new TreeLayout<NodeRef, Arc>();

    DirectedSparseMultigraph<NodeRef, Arc> graph = new DirectedSparseMultigraph<NodeRef, Arc>();

    //final SpringLayout2<NodeRef, Arc> layout = new SpringLayout2<NodeRef, Arc>(graph);
    final KKLayout<NodeRef, Arc> layout = new KKLayout<NodeRef, Arc>(graph);
    final BasicVisualizationServer<NodeRef, Arc> vs = new BasicVisualizationServer<NodeRef, Arc>(layout);

    final JScrollPane scrollPane = new JScrollPane( vs );
    scrollPane.setHorizontalScrollBarPolicy( JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED );
    scrollPane.setVerticalScrollBarPolicy( JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED );

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

        if ( ( g == null && currentGraph == null ) ||
             (g != null && g.equals( currentGraph ) ) )
        {
          return;
        }

        currentGraph = g;

        DirectedSparseMultigraph<NodeRef, Arc> jungGraph = new DirectedSparseMultigraph<NodeRef, Arc>();

        for ( NodeRef ref : currentGraph.getNodeRefs() )
        {
          jungGraph.addVertex( ref );
        }

        for ( Arc arc : currentGraph.getArcs() )
        {
          jungGraph.addEdge( arc, arc.getStartNode(), arc.getEndNode() );
        }

        layout.setGraph( jungGraph );
        scrollPane.repaint();
      }
    } );

    frame.setVisible( true );
  }
}