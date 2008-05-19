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

import org.codemonk.wf.db.HibArc;
import org.codemonk.wf.db.HibEngine;
import org.codemonk.wf.db.HibGraph;
import org.codemonk.wf.db.HibNodeRef;
import org.codemonk.wf.test.TestSetup;
import org.hibernate.Session;

import edu.uci.ics.jung.graph.DirectedSparseMultigraph;
import edu.uci.ics.jung.visualization.BasicVisualizationServer;

public class JungVisualizer
{
  protected static HibGraph currentGraph = null;

  @SuppressWarnings("serial")
  public static void main( String[] args ) throws Exception
  {
    TestSetup.init();

    Session session = TestSetup.openSession();
    HibEngine engine = new HibEngine( session );

    JFrame frame = new JFrame( "Workflow Visualizer" );
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    frame.setMinimumSize(  new Dimension( 800, 600 ) );

    JSplitPane splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT );
    frame.getContentPane().add( splitPane );

    DefaultListModel listModel = new DefaultListModel();
    for ( HibGraph g : engine.getGraphs() )
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
        setText( ((HibGraph)value).getName() );
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

    DirectedSparseMultigraph<HibNodeRef, HibArc> graph = new DirectedSparseMultigraph<HibNodeRef, HibArc>();

    //final SpringLayout2<NodeRef, Arc> layout = new SpringLayout2<NodeRef, Arc>(graph);
    // final KKLayout<HibNodeRef, HibArc> layout = new KKLayout<HibNodeRef, HibArc>(graph);
    final TreeLayout layout = new TreeLayout( graph );
    final BasicVisualizationServer<HibNodeRef, HibArc> vs = new BasicVisualizationServer<HibNodeRef, HibArc>(layout);

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

        final HibGraph g = (HibGraph)graphList.getSelectedValue();

        if ( ( g == null && currentGraph == null ) ||
             (g != null && g.equals( currentGraph ) ) )
        {
          return;
        }

        currentGraph = g;

        DirectedSparseMultigraph<HibNodeRef, HibArc> jungGraph = new DirectedSparseMultigraph<HibNodeRef, HibArc>();

        for ( HibNodeRef ref : currentGraph.getNodeRefs() )
        {
          jungGraph.addVertex( ref );
        }

        for ( HibArc arc : currentGraph.getArcs() )
        {
          jungGraph.addEdge( arc, arc.getStartNode(), arc.getEndNode() );
        }

        GraphTree graphTree = new GraphTree( g );
        layout.setGraph( jungGraph );
        layout.setInitializer( new NodeLocationTransformer( graphTree ) );
        // layout.setGraph( jungGraph );
        scrollPane.repaint();
      }
    } );

    frame.setVisible( true );
  }
}