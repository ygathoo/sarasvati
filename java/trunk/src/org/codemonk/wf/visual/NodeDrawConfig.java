/**
 * Created on May 7, 2008
 */
package org.codemonk.wf.visual;

public class NodeDrawConfig
{
  private static int maxNodeRadius = 25;
  private static int nodeSpacing   = 50;
  private static int anchorSize    = 4;

  public static int getMaxNodeRadius ()
  {
    return maxNodeRadius;
  }

  public static void setMaxNodeRadius (int newMaxNodeRadius)
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

  public static int getAnchorSize()
  {
    return anchorSize;
  }

  public static void setAnchorSize( int anchorSize )
  {
    NodeDrawConfig.anchorSize = anchorSize;
  }


}