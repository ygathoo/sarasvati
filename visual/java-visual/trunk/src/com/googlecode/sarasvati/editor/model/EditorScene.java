package com.googlecode.sarasvati.editor.model;

import org.netbeans.api.visual.widget.LabelWidget;
import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.visual.GraphSceneImpl;

public class EditorScene extends GraphSceneImpl<EditorGraphMember, EditorArc>
{
  @Override
  protected Widget widgetForNode (EditorGraphMember node)
  {
    return new LabelWidget( this, node.getName() );
  }
}
