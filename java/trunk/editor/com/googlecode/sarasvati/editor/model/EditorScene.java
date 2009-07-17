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
package com.googlecode.sarasvati.editor.model;

import java.awt.Color;
import java.awt.Font;
import java.awt.Point;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.JLabel;

import org.netbeans.api.visual.action.ActionFactory;
import org.netbeans.api.visual.action.ConnectProvider;
import org.netbeans.api.visual.action.ConnectorState;
import org.netbeans.api.visual.action.MoveProvider;
import org.netbeans.api.visual.action.ReconnectProvider;
import org.netbeans.api.visual.action.SelectProvider;
import org.netbeans.api.visual.action.WidgetAction;
import org.netbeans.api.visual.anchor.PointShape;
import org.netbeans.api.visual.layout.LayoutFactory.ConnectionWidgetLayoutAlignment;
import org.netbeans.api.visual.model.ObjectSceneEvent;
import org.netbeans.api.visual.model.ObjectSceneEventType;
import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.ConnectionWidget;
import org.netbeans.api.visual.widget.LabelWidget;
import org.netbeans.api.visual.widget.Scene;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.JoinType;
import com.googlecode.sarasvati.editor.GraphEditor;
import com.googlecode.sarasvati.editor.action.ArcPropertiesAction;
import com.googlecode.sarasvati.editor.action.ArcSelectAction;
import com.googlecode.sarasvati.editor.action.ConnectAction;
import com.googlecode.sarasvati.editor.action.GraphMemberMoveAction;
import com.googlecode.sarasvati.editor.action.GraphMemberPropertiesAction;
import com.googlecode.sarasvati.editor.action.GraphMemberSelectAction;
import com.googlecode.sarasvati.editor.action.MoveTrackAction;
import com.googlecode.sarasvati.editor.action.ObjectSceneListenerAdapter;
import com.googlecode.sarasvati.editor.action.SceneAddExternalAction;
import com.googlecode.sarasvati.editor.action.SceneAddNodeAction;
import com.googlecode.sarasvati.editor.command.Command;
import com.googlecode.sarasvati.editor.command.CommandStack;
import com.googlecode.sarasvati.editor.command.DeleteArcCommand;
import com.googlecode.sarasvati.editor.command.DeleteExternalCommand;
import com.googlecode.sarasvati.editor.command.DeleteNodeCommand;
import com.googlecode.sarasvati.editor.command.MoveGraphMemberCommand;
import com.googlecode.sarasvati.editor.command.MultiCommand;
import com.googlecode.sarasvati.editor.command.MultiDeleteCommand;
import com.googlecode.sarasvati.visual.common.GraphSceneImpl;
import com.googlecode.sarasvati.visual.common.NodeDrawConfig;
import com.googlecode.sarasvati.visual.common.PathTrackingConnectionWidget;
import com.googlecode.sarasvati.visual.graph.GraphLayoutNode;
import com.googlecode.sarasvati.visual.icon.AbstractNodeIcon;
import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;
import com.googlecode.sarasvati.visual.icon.TaskIcon;

public class EditorScene extends GraphSceneImpl<EditorGraphMember<?>, EditorArc>
{
  protected static final Font ARC_LABEL_FONT = Font.decode( "serif bold 11" );

  protected final CommandStack commandStack;
  protected final EditorGraph graph;


  private final WidgetAction singleMoveAction = new MoveTrackAction( ActionFactory.createAlignWithMoveAction( mainLayer, intrLayer, null ) );
  private final WidgetAction multiMoveAction = ActionFactory.createMoveAction( null, new MultiMoveProvider() );

  private final GraphMemberMoveAction moveAction = new GraphMemberMoveAction( singleMoveAction );

  private final WidgetAction connectAction = new ConnectAction( ActionFactory.createConnectAction( intrLayer, new SceneConnectProvider() ) );
  private final WidgetAction reconnectAction = ActionFactory.createReconnectAction( new SceneReconnectProvider() );

  private final WidgetAction graphMemberPropertiesAction = new GraphMemberPropertiesAction();
  private final WidgetAction arcPropertiesAction = new ArcPropertiesAction();

  private final WidgetAction arcSelectAction = new ArcSelectAction( createSelectAction() );
  private final WidgetAction graphMemberSelectAction = new GraphMemberSelectAction( createSelectAction() );

  private boolean offsetAddedNodes = false;

  public EditorScene (EditorGraph graph)
  {
    this.graph = graph;
    this.commandStack = new CommandStack();

    getActions().addAction( SceneAddNodeAction.INSTANCE );
    getActions().addAction( SceneAddExternalAction.INSTANCE );
    getActions().addAction( ActionFactory.createRectangularSelectAction( this, this.mainLayer ) );

    addObjectSceneListener( new ObjectSceneListenerAdapter()
    {
      @Override
      public void selectionChanged (final ObjectSceneEvent event,
                                    final Set<Object> previousSelection,
                                    final Set<Object> newSelection)
      {
        setGraphMemberSelection( previousSelection, false );
        setGraphMemberSelection( newSelection, true );
        GraphEditor.getInstance().updateCutCopyPaste( EditorScene.this );
      }
    },
    ObjectSceneEventType.OBJECT_SELECTION_CHANGED );

    for ( EditorGraphMember<?> member : graph.getNodes() )
    {
      addNode( member );
    }

    for ( EditorGraphMember<?> member : graph.getExternals() )
    {
      addNode( member );
    }

    for ( EditorArc arc : graph.getArcs() )
    {
      addEdge( arc );
      setEdgeSource( arc, arc.getStart() );
      setEdgeTarget( arc, arc.getEnd() );
    }

    offsetAddedNodes = true;
  }

  public void setGraphMemberSelection (Set<Object> selection, boolean selected)
  {
    int count = 0;
    for ( Object obj : selection )
    {
      if ( obj instanceof EditorGraphMember )
      {
        ((EditorGraphMember<?>)obj).setSelected( selected );
        count++;
      }
    }

    if ( selected && count > 1 )
    {
      moveAction.setAction( multiMoveAction );
    }
    else
    {
      moveAction.setAction( singleMoveAction );
    }
  }

  public CommandStack getCommandStack ()
  {
    return commandStack;
  }

  public EditorGraph getGraph ()
  {
    return graph;
  }

  public void editCut ()
  {
    removeSelected( "Cut" );
  }

  public void editDelete ()
  {
    removeSelected( "Delete" );
  }

  public boolean isOffsetAddedNodes ()
  {
    return offsetAddedNodes;
  }

  public void setOffsetAddedNodes (boolean offsetAddedNodes)
  {
    this.offsetAddedNodes = offsetAddedNodes;
  }

  public void removeSelected (final String action)
  {
    Set<?> selected = getSelectedObjects();
    List<Command> commands = new ArrayList<Command>( selected.size() );

    Set<EditorArc> arcs = new HashSet<EditorArc>();

    for ( Object obj : getSelectedObjects() )
    {
      if ( obj instanceof EditorNode )
      {
        commands.add( new DeleteNodeCommand( action, this, (EditorNode)obj ) );
      }
      else if ( obj instanceof EditorExternal )
      {
        commands.add( new DeleteExternalCommand( action, this, (EditorExternal)obj ) );
      }
      else if ( obj instanceof EditorArc )
      {
        EditorArc arc = (EditorArc)obj;
        commands.add( new DeleteArcCommand( this, arc ) );
        arcs.add( arc );
      }
    }

    for ( EditorArc arc : graph.getArcs() )
    {
      if ( !arcs.contains( arc ) &&
           (selected.contains( arc.getStart() ) ||
            selected.contains( arc.getEnd() ) ) )
      {
        commands.add( new DeleteArcCommand( this, arc ) );
        arcs.add( arc );
      }
    }

    Collections.sort( commands );

    CommandStack.pushAndPerform( new MultiDeleteCommand( action, this, commands ) );
  }

  @Override
  protected PathTrackingConnectionWidget attachEdgeWidget (final EditorArc arc)
  {
    final PathTrackingConnectionWidget widget = super.attachEdgeWidget( arc );
    widget.setEndPointShape (PointShape.SQUARE_FILLED_BIG);
    widget.getActions().addAction( createObjectHoverAction() );
    widget.getActions().addAction( arcSelectAction );
    widget.getActions().addAction( reconnectAction );
    widget.getActions().addAction( arcPropertiesAction );

    final LabelWidget arcLabel = new LabelWidget( this, arc.getState().getLabel() );
    arcLabel.setFont( ARC_LABEL_FONT );
    arcLabel.setForeground( Color.BLUE );
    arcLabel.setOpaque( true );
    widget.addChild( arcLabel );
    widget.setConstraint( arcLabel, ConnectionWidgetLayoutAlignment.CENTER, 30 );

    arc.addListener( new ModelListener<EditorArc>()
    {
      @Override
      public void modelChanged (final EditorArc modelInstance)
      {
        arcLabel.setLabel( modelInstance.getState().getLabel() );
      }
    });

    return widget;
  }

  // TODO: Add drop-down in node type preferences for icon
  protected AbstractNodeIcon getIconForMember (final EditorGraphMember<?> node)
  {
    boolean join = false;
    boolean isTask = false;

    if ( node instanceof EditorExternal )
    {
      return new DefaultNodeIcon( node.getState().getName(),
                                  NodeDrawConfig.NODE_BG_SKIPPED,
                                  false,
                                  node.isSelected() );
    }

    if ( node instanceof EditorNode )
    {
      join = ((EditorNode)node).getState().getJoinType() != JoinType.OR;
      String typeName = ((EditorNode)node).getState().getType();
      isTask = "task".equalsIgnoreCase( typeName ) || "activity".equalsIgnoreCase( typeName );
    }

    return isTask ? new TaskIcon( node.getState().getName(),
                                  NodeDrawConfig.NODE_BG_COMPLETED,
                                  join,
                                  node.isSelected() ) :
                    new DefaultNodeIcon( node.getState().getName(),
                                         NodeDrawConfig.NODE_BG_COMPLETED,
                                         join,
                                         node.isSelected() );

  }

  @Override
  protected Widget widgetForNode (final EditorGraphMember<?> graphMember)
  {
    final AbstractNodeIcon icon = getIconForMember( graphMember );

    final JLabel label = new JLabel( icon );
    final ComponentWidget widget = new ComponentWidget( this, label );

    if ( offsetAddedNodes )
    {
      int xOffset = icon.getIconWidth() >> 1;
      int yOffset = icon.getIconHeight() >> 1;

      graphMember.setX( graphMember.getX() - xOffset );
      graphMember.setY( graphMember.getY() - yOffset );
    }

    widget.setPreferredLocation( graphMember.getOrigin() );

    widget.getActions().addAction( graphMemberSelectAction );
    widget.getActions().addAction( graphMemberPropertiesAction );
    widget.getActions().addAction( moveAction );
    widget.getActions().addAction( connectAction );

    graphMember.addListener( new ModelListener<EditorGraphMember<?>> ()
    {
      @Override
      public void modelChanged (EditorGraphMember<?> modelInstance)
      {
        label.setIcon( getIconForMember( graphMember ) );
      }
    });

    return widget;
  }

  public class SceneConnectProvider implements ConnectProvider
  {
    private EditorGraphMember<?> source = null;
    private EditorGraphMember<?> target = null;

    public boolean isSourceWidget (Widget sourceWidget)
    {
      Object object = findObject (sourceWidget);
      source = isNode (object) ? (EditorGraphMember<?>) object : null;
      return source != null;
    }

    public ConnectorState isTargetWidget (Widget sourceWidget, Widget targetWidget)
    {
      Object object = findObject (targetWidget);
      target = isNode (object) ? (EditorGraphMember<?>) object : null;
      if (target != null)
      {
        return ConnectorState.ACCEPT;
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
      String arcLabel = sourceWidget == targetWidget ? EditorPreferences.getInstance().getDefalutSelfArcsLabel() : null;
      CommandStack.addArc( EditorScene.this, new EditorArc( new ArcState( arcLabel, null, null ), source, target ) );
    }
  }

  public class SceneReconnectProvider implements ReconnectProvider
  {
    private EditorArc arc;
    private EditorGraphMember<?> originalNode;
    private EditorGraphMember<?> replacementNode;

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
      replacementNode = isNode (object) ? (EditorGraphMember<?>) object : null;
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
        CommandStack.deleteArc( EditorScene.this, arc );
      }
      else if (reconnectingSource)
      {
        CommandStack.updateArc( EditorScene.this, arc, true, replacementNode );
      }
      else
      {
        CommandStack.updateArc( EditorScene.this, arc, false, replacementNode );
      }
    }
  }

  public void autoLayout ()
  {
    EditorGraphLayoutTree layoutTree = new EditorGraphLayoutTree( graph );

    List<Command> commands = new LinkedList<Command>();

    for ( EditorGraphMember<?> member : graph.getNodes() )
    {
      GraphLayoutNode<?> layoutNode = layoutTree.getTreeNode( member );
      Point newOrigin = new Point( layoutNode.getOriginX(), layoutNode.getOriginY() );
      commands.add( new MoveGraphMemberCommand( this, member, member.getOrigin(), newOrigin ) );
    }

    for ( EditorGraphMember<?> member : graph.getExternals() )
    {
      GraphLayoutNode<?> layoutNode = layoutTree.getTreeNode( member );
      Point newOrigin = new Point( layoutNode.getOriginX(), layoutNode.getOriginY() );
      commands.add( new MoveGraphMemberCommand( this, member, member.getOrigin(), newOrigin ) );
    }

    CommandStack.pushAndPerform( new MultiCommand( "Auto-Layout", this, commands ) );
  }

  public class EditorSelectProvider implements SelectProvider
  {
    @Override
    public boolean isAimingAllowed (Widget widget, Point localLocation, boolean invertSelection)
    {
      return true;
    }

    @Override
    public boolean isSelectionAllowed (Widget widget, Point localLocation, boolean invertSelection)
    {
      return true;
    }

    @Override
    public void select (Widget widget, Point localLocation, boolean invertSelection)
    {
      if ( invertSelection )
      {
        System.out.println( "Deselected: " + findObject( widget ) );
      }
      else
      {
        System.out.println( "Selected: " + findObject( widget  ) );
      }
    }
  }

  private class MultiMoveProvider implements MoveProvider
  {
    private Map<Widget, Point> originals = new HashMap<Widget, Point>();
    private Point              original;

    public void movementStarted (Widget widget)
    {
      Object object = findObject( widget );
      if ( isNode( object ) )
      {
        for ( Object o : getSelectedObjects() )
        {
          if ( isNode( o ) )
          {
            Widget w = findWidget( o );
            if ( w != null ) originals.put( w, w.getPreferredLocation() );
          }
        }
      }
      else
      {
        originals.put( widget, widget.getPreferredLocation() );
      }
    }

    public void movementFinished (Widget widget)
    {
      List<Command> commands = new ArrayList<Command>( originals.size() );
      for ( Map.Entry<Widget, Point> entry : originals.entrySet() )
      {
        Point startLocation = new Point( entry.getValue() );
        Point endLocation   = new Point( entry.getKey().getLocation() );

        EditorGraphMember<?> member = (EditorGraphMember<?>) findObject( entry.getKey() );
        member.setOrigin( endLocation );
        commands.add( new MoveGraphMemberCommand( EditorScene.this, member, startLocation, endLocation ) );
      }

      commandStack.pushCommand( new MultiCommand( "Move", EditorScene.this, commands ) );

      originals.clear();
      original = null;
    }

    public Point getOriginalLocation (Widget widget)
    {
      original = widget.getPreferredLocation();
      return original;
    }

    public void setNewLocation (Widget widget, Point location)
    {
      int dx = location.x - original.x;
      int dy = location.y - original.y;
      for ( Map.Entry<Widget, Point> entry : originals.entrySet() )
      {
        Point point = entry.getValue();
        entry.getKey().setPreferredLocation( new Point( point.x + dx, point.y + dy ) );
      }
    }
  }
}