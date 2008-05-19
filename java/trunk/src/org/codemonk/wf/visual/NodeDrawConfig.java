/**
 * Created on May 7, 2008
 */
package org.codemonk.wf.visual;

public class NodeDrawConfig
{
  private static int maxNodeRadius = 25;
  private static int nodeSpacing   = 50;

  public static int getMaxNodeRadius ()
  {
    return maxNodeRadius;
  }

  public static void setMaxNodeSize (int newMaxNodeRadius)
  {
    maxNodeRadius = newMaxNodeRadius;
  }

  public static int getNodeSpacing ()
  {
    return nodeSpacing;
  }

  public static void setNodeSpacing (int newNodeSpacing)
  {
    nodeSpacing = newNodeSpacing;
  }
}