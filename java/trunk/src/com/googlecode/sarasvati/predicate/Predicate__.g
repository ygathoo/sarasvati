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
AFTER : 'after' ;
BEFORE : 'before' ;
DAY : 'day' ;
DAYS : 'days' ;
HOUR : 'hour' ;
HOURS : 'hours' ;
WEEK : 'week' ;
WEEKS : 'weeks' ;
T__32 : '(' ;
T__33 : ')' ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 96
STRING   :  '"' ( '\\\"' | ~( '"' ) )* '"'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 99
ID       :  LETTER ( LETTER | DIGIT | '.' )*
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 102
NUMBER   :  '-'? DIGIT+
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 105
fragment LETTER
         : LOWER | UPPER
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 109
fragment LOWER
         : 'a'..'z'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 113
fragment UPPER
         : 'A'..'Z'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 117
fragment DIGIT
         : '0'..'9'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 121
NEWLINE  :   ('\r'? '\n')+ { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 124
WHITESPACE
         : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 128
SINGLE_COMMENT
         : '//' ~('\r' | '\n')* NEWLINE { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 132
MULTI_COMMENT options { greedy = false; }
         : '/*' .* '*/' NEWLINE? { $channel=HIDDEN; }
         ;
