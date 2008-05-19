package org.codemonk.wf.visual;

import java.awt.geom.Point2D;

import org.apache.commons.collections15.Transformer;
import org.codemonk.wf.db.HibArc;
import org.codemonk.wf.db.HibNodeRef;

import edu.uci.ics.jung.algorithms.layout.AbstractLayout;
import edu.uci.ics.jung.graph.Graph;

public class TreeLayout extends AbstractLayout<HibNodeRef, HibArc>
{
  protected TreeLayout (Graph<HibNodeRef, HibArc> graph)
  {
    super( graph );
  }

  protected TreeLayout (Graph<HibNodeRef, HibArc> graph, Transformer<HibNodeRef, Point2D> trans)
  {
    super( graph, trans );
  }

  @Override
  public void initialize ()
  {
  }

  @Override
  public void reset ()
  {
  }
}
