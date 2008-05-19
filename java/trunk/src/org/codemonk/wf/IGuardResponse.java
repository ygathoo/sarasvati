package org.codemonk.wf;

public interface IGuardResponse
{
  static IGuardResponse ACCEPT_TOKEN_RESPONSE = new IGuardResponse()
  {
    @Override
    public String getExitArcForSkip()
    {
      throw new UnsupportedOperationException(
        "getExitArcsForSkip should never be called on a GuardResponse with action of AcceptToken" );
    }

    @Override
    public GuardAction getGuardAction()
    {
      return GuardAction.AcceptToken;
    }
  };

  static IGuardResponse DISCARD_TOKEN_RESPONSE = new IGuardResponse()
  {
    @Override
    public String getExitArcForSkip()
    {
      throw new UnsupportedOperationException(
        "getExitArcsForSkip should never be called on a GuardResponse with action of DiscardToken" );

    }

    @Override
    public GuardAction getGuardAction()
    {
      return GuardAction.DiscardToken;
    }
  };

  GuardAction getGuardAction ();

  String getExitArcForSkip ();
}
