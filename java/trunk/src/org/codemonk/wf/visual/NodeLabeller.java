/**
 * Created on May 15, 2008
 */
package org.codemonk.wf.visual;

import org.apache.commons.collections15.Transformer;
import org.codemonk.wf.db.HibNodeRef;

public class NodeLabeller implements Transformer<HibNodeRef,String>
{
  @Override
  public String transform (HibNodeRef nodeRef)
  {
    return nodeRef.getLabel();
  }
}
