lexer grammar Predicate;

@header {
package com.googlecode.sarasvati.predicate;
}

IF : 'if' ;
THEN : 'then' ;
ELSE : 'else' ;
OR : 'or' ;
AND : 'and' ;
NOT : 'not' ;
ACCEPT : 'Accept' ;
DISCARD : 'Discard' ;
SKIP : 'Skip' ;
NOW : 'now' ;
AFTER : 'after' ;
BEFORE : 'before' ;
T__27 : '(' ;
T__28 : ')' ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 88
STRING   :  '"' ( '\\\"' | ~( '"' ) )* '"'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 91
ID       :  LETTER ( LETTER | DIGIT | '.' )*
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 94
NUMBER   :  DIGIT+
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 97
fragment LETTER
         : LOWER | UPPER
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 101
fragment LOWER
         : 'a'..'z'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 105
fragment UPPER
         : 'A'..'Z'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 109
fragment DIGIT
         : '0'..'9'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 113
NEWLINE  :   ('\r'? '\n')+ { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 116
WHITESPACE
         : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 120
SINGLE_COMMENT
         : '//' ~('\r' | '\n')* NEWLINE { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 124
MULTI_COMMENT options { greedy = false; }
         : '/*' .* '*/' NEWLINE? { $channel=HIDDEN; }
         ;
