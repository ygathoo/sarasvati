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
package com.googlecode.sarasvati.editor.model;

import java.awt.Point;

import javax.swing.Icon;
import javax.swing.JLabel;

import org.netbeans.api.visual.action.ActionFactory;
import org.netbeans.api.visual.action.ConnectProvider;
import org.netbeans.api.visual.action.ConnectorState;
import org.netbeans.api.visual.action.ReconnectProvider;
import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.Scene;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.editor.EditorMode;
import com.googlecode.sarasvati.editor.GraphEditor;
import com.googlecode.sarasvati.editor.SceneAddNodeAction;
import com.googlecode.sarasvati.visual.common.GraphSceneImpl;
import com.googlecode.sarasvati.visual.common.NodeDrawConfig;
import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;

public class EditorScene extends GraphSceneImpl<EditorGraphMember, EditorArc>
{
  private GraphEditor editor;
  private EditorGraph graph;

  final private WidgetAction addNodeAction = new SceneAddNodeAction( this );
  final private WidgetAction moveAction = ActionFactory.createAlignWithMoveAction( mainLayer, intrLayer, null );
  final private WidgetAction connectAction = ActionFactory.createConnectAction( intrLayer, new SceneConnectProvider() );
  final private WidgetAction reconnectAction = ActionFactory.createReconnectAction( new SceneReconnectProvider() );

  public EditorScene (GraphEditor editor, EditorGraph graph)
  {
    this.editor = editor;
    this.graph = graph;

    for ( EditorGraphMember member : graph.getMembers().values() )
    {
      addNode( member );
    }

    for ( EditorArc arc : graph.getArcs() )
    {
      addEdge( arc );
      setEdgeSource( arc, arc.getStart() );
      setEdgeTarget( arc, arc.getEnd() );
    }
  }

  public void modeAddNode ()
  {
    getActions().addAction( addNodeAction );

    for (Widget widget : mainLayer.getChildren() )
    {
      widget.getActions().removeAction( moveAction );
      widget.getActions().addAction( connectAction );
    }
  }

  public void modeMove ()
  {
    getActions().removeAction( addNodeAction );

    for ( Widget widget : mainLayer.getChildren() )
    {
      widget.getActions().removeAction( connectAction );
      widget.getActions().addAction( moveAction );
    }
  }

  @Override
  protected Widget attachEdgeWidget (EditorArc edge)
  {
    Widget widget = super.attachEdgeWidget( edge );
    widget.getActions().addAction( createObjectHoverAction() );
    widget.getActions().addAction( createSelectAction() );
    widget.getActions().addAction( reconnectAction );
    return widget;
  }

  @Override
  protected Widget widgetForNode (EditorGraphMember node)
  {
    boolean join = false;

    if ( node instanceof EditorNode )
    {
      join = ((EditorNode)node).isJoin();
    }

    Icon icon = new DefaultNodeIcon( node.getName(), NodeDrawConfig.NODE_BG_COMPLETED, join );

    JLabel label = new JLabel( icon );
    ComponentWidget widget = new ComponentWidget( this, label );

    int xOffset = icon.getIconWidth() >> 1;
    int yOffset = icon.getIconHeight() >> 1;

    node.setX( node.getX() - xOffset );
    node.setY( node.getY() - yOffset );

    widget.setPreferredLocation( node.getOrigin() );

    if ( editor.getMode() == EditorMode.Move )
    {
      widget.getActions().addAction( moveAction );
    }
    else if ( editor.getMode() == EditorMode.AddNode )
    {
      widget.getActions().addAction( connectAction );
    }

    return widget;
  }

  private class SceneConnectProvider implements ConnectProvider
  {
    private EditorGraphMember source = null;
    private EditorGraphMember target = null;

    public boolean isSourceWidget (Widget sourceWidget)
    {
      System.out.println( "Hello" );
      Object object = findObject (sourceWidget);
      source = isNode (object) ? (EditorGraphMember) object : null;
      return source != null;
    }

    public ConnectorState isTargetWidget (Widget sourceWidget, Widget targetWidget)
    {
      Object object = findObject (targetWidget);
      target = isNode (object) ? (EditorGraphMember) object : null;
      if (target != null)
      {
        return !source.equals (target) ? ConnectorState.ACCEPT : ConnectorState.REJECT_AND_STOP;
      }
      return object != null ? ConnectorState.REJECT_AND_STOP : ConnectorState.REJECT;
    }

    public boolean hasCustomTargetWidgetResolver (Scene scene)
    {
      return false;
    }

    public Widget resolveTargetWidget (Scene scene, Point sceneLocation)
    {
      return null;
    }

    public void createConnection (Widget sourceWidget, Widget targetWidget)
    {
      EditorArc arc = new EditorArc();
      arc.setStart( source );
      arc.setEnd( target );

      graph.addArc( arc );

      addEdge( arc );
      setEdgeSource( arc, source );
      setEdgeTarget( arc, target );
    }
  }

  private class SceneReconnectProvider implements ReconnectProvider
  {
    private EditorArc arc;
    private EditorGraphMember originalNode;
    private EditorGraphMember replacementNode;

    public void reconnectingStarted (ConnectionWidget connectionWidget, boolean reconnectingSource)
    {
      // does nothing
    }

    public void reconnectingFinished (ConnectionWidget connectionWidget, boolean reconnectingSource)
    {
      // does nothing
    }

    public boolean isSourceReconnectable (ConnectionWidget connectionWidget)
    {
      Object object = findObject (connectionWidget);
      arc = isEdge (object) ? (EditorArc) object : null;
      originalNode = arc != null ? getEdgeSource (arc) : null;
      return originalNode != null;
    }

    public boolean isTargetReconnectable (ConnectionWidget connectionWidget)
    {
      Object object = findObject (connectionWidget);
      arc = isEdge (object) ? (EditorArc) object : null;
      originalNode = arc != null ? getEdgeTarget (arc) : null;
      return originalNode != null;
    }

    public ConnectorState isReplacementWidget (ConnectionWidget connectionWidget, Widget replacementWidget, boolean reconnectingSource)
    {
      Object object = findObject (replacementWidget);
      replacementNode = isNode (object) ? (EditorGraphMember) object : null;
      if (replacementNode != null)
      {
        return ConnectorState.ACCEPT;
      }
      return object != null ? ConnectorState.REJECT_AND_STOP : ConnectorState.REJECT;
    }

    public boolean hasCustomReplacementWidgetResolver (Scene scene)
    {
        return false;
    }

    public Widget resolveReplacementWidget (Scene scene, Point sceneLocation)
    {
        return null;
    }

    public void reconnect (ConnectionWidget connectionWidget, Widget replacementWidget, boolean reconnectingSource)
    {
      if (replacementWidget == null)
      {
        graph.removeArc( arc );
        removeEdge( arc );
      }
      else if (reconnectingSource)
      {
        arc.setStart( replacementNode );
        setEdgeSource( arc, replacementNode );
      }
      else
      {
        arc.setEnd( replacementNode );
        setEdgeTarget( arc, replacementNode );
      }
    }
  }
}