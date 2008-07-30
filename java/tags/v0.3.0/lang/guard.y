%{
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

import org.codemonk.wf.*;

public class GuardLangParser
{
%}

%token SYMBOL
%token AND OR NOT
%token IF THEN ELSE
%token ACCEPT DISCARD SKIP

%type <String> SYMBOL

%type <GuardStmt> Stmt
%type <GuardExpr> Expr
%type <GuardExpr> UnitExpr
%type <GuardResponse> Result

%left AND OR

%nonassoc ELSE

%start Stmt

%%

Stmt : IF Expr THEN Stmt ELSE Stmt { $$ = new GuardStmtIf( $2, $4, $6 ); }
     | Result                      { $$ = new GuardStmtResult( $1 ); }
     ;

Expr : Expr OR Expr  { $$ = new GuardExprOr( $1, $3 );  }
     | Expr AND Expr { $$ = new GuardExprAnd( $1, $3 ); }
     | NOT UnitExpr  { $$ = new GuardExprNot( $2 );     }
     | UnitExpr      { $$ = $1;                         }
     ;

UnitExpr : SYMBOL       { $$ = new GuardExprSymbol( $1 );   }
         | '(' Expr ')' { $$ = $2;                          }
         ;

Result : ACCEPT      { $$ = GuardResponse.ACCEPT_TOKEN_RESPONSE;  }
       | DISCARD     { $$ = GuardResponse.DISCARD_TOKEN_RESPONSE; }
       | SKIP SYMBOL { $$ = new SkipNodeGuardResponse( $2 ); }
       | SKIP        { $$ = SkipNodeGuardResponse.DEFAULT_ARC_SKIP_NODE_RESPONSE;  }
       ;

%%

}