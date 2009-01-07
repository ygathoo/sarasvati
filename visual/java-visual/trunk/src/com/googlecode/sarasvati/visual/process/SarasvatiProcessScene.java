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
import java.awt.Point;

import javax.swing.JLabel;

import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.ArcToken;
import com.googlecode.sarasvati.GraphProcess;
import com.googlecode.sarasvati.visual.GraphSceneImpl;
import com.googlecode.sarasvati.visual.NodeDrawConfig;
import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.icon.TaskIcon;

public class SarasvatiProcessScene extends GraphSceneImpl<ProcessTreeNode, ProcessTreeArc>
{
  protected GraphProcess process;
  protected ProcessTree processTree;
  protected boolean showSelfArcs;

  public SarasvatiProcessScene (GraphProcess process)
  {
    this( process, false );
  }

  public SarasvatiProcessScene (GraphProcess process, boolean showSelfArcs)
  {
    this.showSelfArcs = showSelfArcs;
    this.process = process;

    if ( process != null )
    {
      ProcessTree pt = new ProcessTree( process );
      Iterable<ProcessTreeNode> nodes = pt.getProcessTreeNodes();

      for ( ProcessTreeNode node : nodes )
      {
        addNode( node );
        Widget widget = findWidget( node );

        Point origin = new Point( node.getOriginX(), node.getOriginY() );
        widget.setPreferredLocation( origin );
        widget.resolveBounds( origin, null );
      }

      for ( ProcessTreeNode node : nodes )
      {
        for ( ProcessTreeArc ptArc : node.getChildren() )
        {
          if ( showSelfArcs || !ptArc.getParent().equals( ptArc.getChild() ) )
          {
            addEdge( ptArc );
            setEdgeSource( ptArc, ptArc.getParent() );
            setEdgeTarget( ptArc, ptArc.getChild() );

            ConnectionWidget w = (ConnectionWidget)findWidget( ptArc );
            w.resolveBounds( null, null );

            ArcToken token =  ptArc.getToken();
            if ( token != null )
            {
              w.setStroke( new BasicStroke( 3 ) );
              if ( token.getExecutionType().isBacktracked() )
              {
                w.setLineColor( NodeDrawConfig.NODE_BG_BACKTRACKED );
              }
              else if ( token.isComplete() )
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
      }
    }

    revalidate();
  }

  @Override
  protected Widget widgetForNode (ProcessTreeNode ptNode)
  {
    JLabel label = null;

    if ( "task".equals( ptNode.getNode().getType() ) )
    {
      label =  new JLabel( new TaskIcon( ptNode.getNode(), ptNode.getToken() ) );
    }
    else
    {
      label = new JLabel( new DefaultNodeIcon( ptNode.getNode(), ptNode.getToken() ) );
    }

    return new ComponentWidget( this, label );
  }
}