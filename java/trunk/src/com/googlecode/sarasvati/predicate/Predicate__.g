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

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 95
STRING   :  '"' ( '\\\"' | ~( '"' ) )* '"'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 98
ID       :  LETTER ( LETTER | DIGIT | '.' )*
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 101
NUMBER   :  '-'? DIGIT+
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 104
fragment LETTER
         : LOWER | UPPER
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 108
fragment LOWER
         : 'a'..'z'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 112
fragment UPPER
         : 'A'..'Z'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 116
fragment DIGIT
         : '0'..'9'
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 120
NEWLINE  :   ('\r'? '\n')+ { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 123
WHITESPACE
         : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+ { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 127
SINGLE_COMMENT
         : '//' ~('\r' | '\n')* NEWLINE { $channel=HIDDEN; }
         ;

// $ANTLR src "src/com/googlecode/sarasvati/predicate/Predicate.g" 131
MULTI_COMMENT options { greedy = false; }
         : '/*' .* '*/' NEWLINE? { $channel=HIDDEN; }
         ;
