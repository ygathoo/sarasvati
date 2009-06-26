/**
 * Created on Jun 26, 2009
 */
package com.googlecode.sarasvati.editor.model;

import com.googlecode.sarasvati.JoinType;

public class NodeState extends GraphMemberState
{
  private final String  type;
  private final JoinType joinType;
  private final String joinParam;
  private final boolean isStart;
  private final String  guard;

  public NodeState (final String name,
                    final String type,
                    final JoinType joinType,
                    final String joinParam,
                    final boolean isStart,
                    final String guard)
  {
    super( name );
    this.type = type;
    this.joinType = joinType;
    this.joinParam = joinParam;
    this.isStart = isStart;
    this.guard = guard;
  }

  public String getType ()
  {
    return type;
  }

  public JoinType getJoinType ()
  {
    return joinType;
  }

  public String getJoinParam ()
  {
    return joinParam;
  }

  public boolean isStart ()
  {
    return isStart;
  }

  public String getGuard ()
  {
    return guard;
  }
}
