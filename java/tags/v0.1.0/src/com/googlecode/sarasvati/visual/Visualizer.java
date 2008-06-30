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

import org.hibernate.Session;

import com.googlecode.sarasvati.example.db.TestSetup;
import com.googlecode.sarasvati.hib.HibEngine;
import com.googlecode.sarasvati.hib.HibGraph;

public class Visualizer
{
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

    final GraphDrawing graphDrawing = new GraphDrawing();
    graphDrawing.setBackground( Color.white );
    graphDrawing.setOpaque( true );

    final JScrollPane scrollPane = new JScrollPane( graphDrawing );
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

        if ( ( g == null && graphDrawing.getGraph() == null ) ||
             (g != null && g.equals( graphDrawing.getGraph() ) ) )
        {
          return;
        }

        graphDrawing.setGraph( g );
        scrollPane.repaint();
      }
    } );

    frame.setVisible( true );
  }
}