package org.codemonk.wf;

public interface GuardResponse
{
  static GuardResponse ACCEPT_TOKEN_RESPONSE = new GuardResponse()
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

  static GuardResponse DISCARD_TOKEN_RESPONSE = new GuardResponse()
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
