/**
 * Created on May 15, 2008
 */
package org.codemonk.wf.visual;

import org.apache.commons.collections15.Transformer;
import org.codemonk.wf.db.HibArc;

public class ArcLabeller implements Transformer<HibArc,String>
{
  @Override
  public String transform (HibArc arc)
  {
    return arc.getName();
  }
}
