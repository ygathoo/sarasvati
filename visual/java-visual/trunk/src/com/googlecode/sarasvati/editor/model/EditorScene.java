package com.googlecode.sarasvati.editor.model;

import org.netbeans.api.visual.widget.Widget;

import com.googlecode.sarasvati.visual.GraphSceneImpl;

public class EditorScene extends GraphSceneImpl<EditorNode, EditorArc>
{
  @Override
  protected Widget widgetForNode (EditorNode node)
  {
    return null;
  }
}
