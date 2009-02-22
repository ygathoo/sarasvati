package com.googlecode.sarasvati.editor.model;

import javax.swing.JLabel;

import org.netbeans.api.visual.widget.ComponentWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.visual.common.GraphSceneImpl;
import com.googlecode.sarasvati.visual.common.NodeDrawConfig;
import com.googlecode.sarasvati.visual.icon.DefaultNodeIcon;

public class EditorScene extends GraphSceneImpl<EditorGraphMember, EditorArc>
{
  @Override
  protected Widget widgetForNode (EditorGraphMember node)
  {
    boolean join = false;

    if ( node instanceof EditorNode )
    {
      join = ((EditorNode)node).isJoin();
    }

    JLabel label = new JLabel( new DefaultNodeIcon( node.getName(), NodeDrawConfig.NODE_BG_COMPLETED, join ) );
    return new ComponentWidget( this, label );
  }
}
