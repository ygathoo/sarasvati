/**
 * Created on May 7, 2008
 */
package org.codemonk.wf.visual.painter;

import java.util.HashMap;
import java.util.Map;

public class NodePainterFactory
{
  protected static final Map<String, NodePainter> painterMap = new HashMap<String, NodePainter>();
  protected static final NodePainter defaultPainter = new DefaultNodePainter();

  static
  {
    painterMap.put( "start", new StartNodePainter() );
    painterMap.put( "task", new TaskNodePainter() );
  }

  public static NodePainter getInstance (String type)
  {
    NodePainter painter = painterMap.get( type );
    return painter == null ? defaultPainter : painter;
  }
}
