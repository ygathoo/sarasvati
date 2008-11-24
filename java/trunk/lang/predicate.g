grammar predicate;

tokens {
	IF	=	'if';
	THEN	=	'then';
	ELSE	=	'else';
	OR	=	'or';
	AND	=	'and';
	NOT	=	'not';
	ACCEPT  = 	'Accept';
	DISCARD = 	'Discard';
	SKIP	= 	'Skip';	
	NOW     = 	'now';
	AFTER   = 	'after';
	BEFORE  =	'before';
}

program	:	stmt^ EOF!
	;

stmt	:	result | IF^ orExpr THEN! stmt ELSE! stmt
	;

orExpr	:	andExpr ( OR^ andExpr )*
	;

andExpr	:	notExpr ( AND^ notExpr )*
	;

notExpr :	NOT^? expr
	;

expr	:	ID | '('! orExpr ')'!
	;

ID	:	LETTER ( LETTER | DIGIT | '.' )*
	;

result  :	guardResult | DIGIT+ | ID
	;

guardResult 
	:	ACCEPT | DISCARD | SKIP^ ID?
	;

// ===========================================================================
// ================   LEXER   ================================================
// ===========================================================================

//QSTRING :	'"' ( '\"' | ~( '\\' | '"' )* '"' 
//        ;
        
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
	
NEWLINE : 	('\r'? '\n')+ { $channel=HIDDEN; }
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
