/*
    This file is part of Sarasvati.

    Sarasvati is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as
    published by the Free Software Foundation, either version 3 of the
    License, or (at your option) any later version.

    Sarasvati is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with Sarasvati.  If not, see <http://www.gnu.org/licenses/>.

    Copyright 2008 Paul Lorenz
*/
package com.googlecode.sarasvati;

import com.googlecode.sarasvati.impl.AndJoinStrategy;
import com.googlecode.sarasvati.impl.LabelAndJoinStrategy;
import com.googlecode.sarasvati.impl.OrJoinStrategy;
import com.googlecode.sarasvati.impl.TokenSetAndJoinStrategy;

/**
 * Enumerates the types of joins.
 *
 * @author Paul Lorenz
 */
public enum JoinType
{
  /**
   * Uses the {@link OrJoinStrategy}.
   * A join of this type will be satisfied any time
   * an arc token arrives at the node.
   */
  OR( new OrJoinStrategy(),
      "An OR join will be satisfied any time an arc token arrives at the node." ),

  /**
   * Uses the {@link AndJoinStrategy}. A join of this type
   * will be satisfied when an arc token arrives, and there
   * are arc tokens waiting at all other incoming arcs to
   * the node.
   */
  AND( new AndJoinStrategy(),
       "An AND join will be satisfied when an arc token arrives and there are " +
       "arc tokens waiting at all other incoming arcs to the node."),

  /**
   * Uses the {@link LabelAndJoinStrategy}. A join of this type
   * will be satisfied when an arc token arrives, and there
   * are arc tokens waiting at all other incoming arcs to the
   * node which share the same name/label as the arc that the
   * arc token is arriving on.
   */
  LABEL_AND( new LabelAndJoinStrategy(),
             "A LABEL_AND join will be satisfied when an arc token arrives and there are " +
             "arc tokens waiting at all other incoming arcs to the node which " +
             "share the same name/label as the arc that the arc token is arriving on." ),

  /**
   * Uses the {@link TokenSetAndJoinStrategy}. A join of this type
   * will be satisfied when an arc token arrives, and all the other
   * active arc tokens in the set are on incoming arcs to the same node
   * and there are no active node tokens.
   */
  TOKEN_SET_AND( new TokenSetAndJoinStrategy(),
                 "A TOKEN_SET_AND join will be satisfied when an arc token arrives and all the " +
                 "other active arc tokens in the set are on incoming arcs to the same node and " +
                 "there are no active node tokens in the token set." ),

  /**
   * Users may use custom join strategies. This can be done by overriding
   * {@link Node#getJoinStrategy()}. Alternately, the node type of custom
   * may be used, and a join strategy can be set into the CUSTOM join type.
   */
  CUSTOM( new JoinStrategy()
          {
            @Override
            public JoinResult performJoin (final Engine engine, final GraphProcess process, final ArcToken token)
            {
              throw new UnsupportedOperationException( "No custom join strategy has been set. " +
                                                       "If not overriding Node#getJoinStrategy, " +
                                                       "make sure to invoke JoinType.CUSTOM.setJoinStrategy " +
                                                       "before using a custom join type." );
            }
          },
         "A CUSTOM join will be satified based on the join strategy implemented by the user." )
  ;

  private JoinStrategy joinStrategy;
  private String description;

  private JoinType (final JoinStrategy joinStrategy, final String description)
  {
    this.joinStrategy = joinStrategy;
    this.description = description;
  }

  public JoinStrategy getJoinStrategy ()
  {
    return joinStrategy;
  }

  /**
   * Allows changing the JoinStrategy for the CUSTOM NodeType only.
   *
   * @param joinStrategy The new join strategy to use
   */
  public void setJoinStrategy (final JoinStrategy joinStrategy)
  {
    if ( this != CUSTOM )
    {
      throw new IllegalArgumentException( "The join strategy may only be changed on the CUSTOM join type" );
    }
    this.joinStrategy = joinStrategy;
  }

  public String getDescription ()
  {
    return description;
  }
}