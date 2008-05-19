package org.codemonk.wf;

public class SkipNodeGuardResponse implements IGuardResponse
{
  public static final SkipNodeGuardResponse DEFAULT_ARC_SKIP_NODE_RESPONSE = new SkipNodeGuardResponse( IArc.DEFAULT_ARC );

  protected String exitArcForSkip = null;

  public SkipNodeGuardResponse (String arcName)
  {
    this.exitArcForSkip = arcName;
  }

  @Override
  public final GuardAction getGuardAction()
  {
    return GuardAction.SkipNode;
  }

  @Override
  public String getExitArcForSkip()
  {
    return exitArcForSkip;
  }
}
