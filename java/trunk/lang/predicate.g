grammar predicate;


stmt	:	'if' orexpr 'then' 'test';

orexpr	:	andexpr ( 'or' andexpr )*;

andexpr	:	notexpr ( 'and' notexpr )*;

notexpr :	'not'? pred;

pred	:	LETTER ( LETTER | DIGIT | '.' )*;

fragment LETTER: LOWER | UPPER;
fragment LOWER: 'a'..'z';
fragment UPPER: 'A'..'Z';
fragment DIGIT: '0'..'9';
	
NEWLINE : 	('\r'? '\n')+;
WHITESPACE 
	: ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+;

SINGLE_COMMENT
  	: '//' ~('\r' | '\n')* NEWLINE { skip(); };

MULTI_COMMENT options { greedy = false; }
	: '/*' .* '*/' NEWLINE? { skip(); };
