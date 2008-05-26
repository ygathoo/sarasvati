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

import java.io.IOException;

import org.codemonk.wf.GuardResponse;

public class GuardLang
{
  public static GuardResponse eval (String text, GuardEnv env) throws GuardException
  {
    try
    {
      GuardStmt stmt = (GuardStmt)new GuardLangParser().yyparse( new GuardLangLexer( text ) );
      return stmt.eval( env );
    }
    catch (IOException ioe )
    {
      throw new GuardException( text, "Failed to execute script", ioe );
    }
    catch ( GuardLangParser.yyException ye )
    {
      throw new GuardException( text, "Failed to execute script", ye );
    }
  }
}
