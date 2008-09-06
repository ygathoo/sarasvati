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
package com.googlecode.sarasvati.visual;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Point;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.ListCellRenderer;
import javax.swing.ListSelectionModel;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.hibernate.Session;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.Arc;
import com.googlecode.sarasvati.Graph;
import com.googlecode.sarasvati.Node;
import com.googlecode.sarasvati.adapter.Function;
import com.googlecode.sarasvati.adapter.NodeAdapterManager;
import com.googlecode.sarasvati.example.db.TestSetup;
import com.googlecode.sarasvati.hib.HibEngine;

public class GraphVisualizer
{
  protected static Graph currentGraph = null;
  protected static SarasvatiScene scene = new SarasvatiScene();

  @SuppressWarnings("serial")
  public static void main( String[] args ) throws Exception
  {
    TestSetup.init();

    NodeAdapterManager.registerFactory( Component.class,
        new Function<Node, Component>()
        {
          @Override
          public Component apply (Node node)
          {
            if ( "task".equals( node.getType() ) )
            {
              //return new ComponentWidget( this, new TaskComponent( node ) );
              return new JLabel( new TaskIcon( node ) );
            }

            return new JLabel( new DefaultNodeIcon( node ) );
          }
        });

    Session session = TestSetup.openSession();
    HibEngine engine = new HibEngine( session );

    JFrame frame = new JFrame( "Workflow Visualizer" );
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    frame.setMinimumSize(  new Dimension( 800, 600 ) );

    JSplitPane splitPane = new JSplitPane( JSplitPane.HORIZONTAL_SPLIT );
    frame.getContentPane().add( splitPane );

    DefaultListModel listModel = new DefaultListModel();
    for ( Graph g : engine.getRepository().getGraphs() )
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

        Graph g = (Graph)value;

        setText( g.getName() + "." + g.getVersion() + "  " );
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

    final JScrollPane scrollPane = new JScrollPane();
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

        final Graph g = (Graph)graphList.getSelectedValue();

        if ( ( g == null && currentGraph == null ) ||
             ( g != null && g.equals( currentGraph ) ) )
        {
          return;
        }

        currentGraph = g;
        scene = new SarasvatiScene();

        for ( Node node : scene.getNodes() )
        {
          scene.removeNodeWithEdges( node );
        }

        for ( Node ref : currentGraph.getNodes() )
        {
          scene.addNode( ref );
        }

        for ( Arc arc : currentGraph.getArcs() )
        {
          scene.addEdge( arc );
          scene.setEdgeSource( arc, arc.getStartNode() );
          scene.setEdgeTarget( arc, arc.getEndNode() );
        }

        GraphTree graphTree = new GraphTree( g );

        for ( Node node : currentGraph.getNodes() )
        {
          Widget widget = scene.findWidget( node );
          GraphTreeNode treeNode = graphTree.getTreeNode( node );
          widget.setPreferredLocation( new Point( treeNode.getOriginX(), treeNode.getOriginY() ) );
        }

        scrollPane.setViewportView( scene.createView() );
        scrollPane.repaint();
      }
    } );

    frame.setVisible( true );
  }
}