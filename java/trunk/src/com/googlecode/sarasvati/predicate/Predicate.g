grammar Predicate;

options {
   output=AST;
   ASTLabelType=CommonTree;
}

tokens {
  IF  =  'if';
  THEN  =  'then';
  ELSE  =  'else';
  OR  =  'or';
  AND  =  'and';
  NOT  =  'not';
  ACCEPT  =   'Accept';
  DISCARD =   'Discard';
  SKIP  =   'Skip';
  NOW     =   'now';
  AFTER   =   'after';
  BEFORE  =  'before';
}

@header {
package com.googlecode.sarasvati.predicate;

import com.googlecode.sarasvati.GuardResponse;
import com.googlecode.sarasvati.SkipNodeGuardResponse;
}

@lexer::header {
package com.googlecode.sarasvati.predicate;
}

program returns [PredicateStmt value]
         :  stmt EOF { $value = $stmt.value; }
         ;

stmt returns [PredicateStmt value]
         :  IF^ e=orExpr THEN! ifStmt=stmt ELSE! elseStmt=stmt {$value = new PredicateIf( $e.value, $ifStmt.value, $elseStmt.value ); }
         |  result -> result
         ;

orExpr returns [PredicateExpr value]
         :  left=andExpr { $value = $left.value; } ( OR^ right=andExpr { $value = new PredicateExprOr( $value, $right.value ); } )*
         ;

andExpr returns [PredicateExpr value]
         :  left=notExpr { $value = $left.value; } ( AND^ right=notExpr { $value = new PredicateExprAnd( $value, $right.value ); } )*
         ;

notExpr returns [PredicateExpr value]
         :  NOT expr { $value = new PredicateExprNot( $expr.value ); }
         |  expr { $value = $expr.value; }
         ;

expr returns [PredicateExpr value]
         :  ID { $value = new PredicateExprSymbol( $ID.text ); }
         |  '('! orExpr ')'! { $value = $orExpr.value; }
         ;

result   :  guardResult
         |  NUMBER
         |  ID
         |  STRING
         |  dateResult
         ;

dateResult
         :  '('! dateSpec ')'!
         ;

dateSpec :  NOW
         |  ID
         |  NUMBER (BEFORE|AFTER) ID
         ;

guardResult returns [GuardResponse value]
         :  ACCEPT   { $value = GuardResponse.ACCEPT_TOKEN_RESPONSE; }
         |  DISCARD  { $value = GuardResponse.DISCARD_TOKEN_RESPONSE; }
         |  SKIP^ ID { $value = new SkipNodeGuardResponse( $ID.text ); }
         |  SKIP     { $value = SkipNodeGuardResponse.DEFAULT_ARC_SKIP_NODE_RESPONSE; }
         ;

// ===========================================================================
// ================   LEXER   ================================================
// ===========================================================================

STRING   :  '"' ( '\\\"' | ~( '"' ) )* '"'
         ;

ID       :  LETTER ( LETTER | DIGIT | '.' )*
         ;

NUMBER   :  DIGIT+
         ;

fragment LETTER
         : LOWER | UPPER
         ;

fragment LOWER
         : 'a'..'z'
         ;

fragment UPPER
         : 'A'..'Z'
         ;

fragment DIGIT
         : '0'..'9'
         ;

NEWLINE  :   ('\r'? '\n')+ { $channel=HIDDEN; }
         ;

WHITESPACE
         : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { $channel=HIDDEN; }
         ;

SINGLE_COMMENT
         : '//' ~('\r' | '\n')* NEWLINE { $channel=HIDDEN; }
         ;

MULTI_COMMENT options { greedy = false; }
         : '/*' .* '*/' NEWLINE? { $channel=HIDDEN; }
         ;
