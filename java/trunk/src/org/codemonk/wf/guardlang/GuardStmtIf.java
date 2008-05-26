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

package org.codemonk.wf.guardlang;

import org.codemonk.wf.GuardResponse;

public class GuardStmtIf implements GuardStmt
{
  protected GuardExpr expr;
  protected GuardStmt ifStmt;
  protected GuardStmt elseStmt;

  public GuardStmtIf (GuardExpr expr, GuardStmt ifStmt, GuardStmt elseStmt)
  {
    this.expr = expr;
    this.ifStmt = ifStmt;
    this.elseStmt = elseStmt;
  }

  @Override
  public GuardResponse eval( GuardEnv env )
  {
    return expr.eval( env ) ? ifStmt.eval(  env ) : elseStmt.eval( env );
  }
}
