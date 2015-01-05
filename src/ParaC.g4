grammar ParaC;

WS
:
	[ \t\r\n]+ -> skip
;

IDENTIFIER
:
	[a-z_] [a-zA-Z_]*
;

FCONSTANT
:
	[0-9]+ '.' [0-9]*
	| [0-9]* '.' [0-9]+
;

ICONSTANT
:
	[0-9]+
;

primaryExpression
:
	IDENTIFIER # primaryExpressionWithIdentifier
	| ICONSTANT # primaryExpressionWithInteger
	| FCONSTANT # primaryExpressionWithFloat
	| '(' expression ')' # primaryExpressionWithExpression
	| IDENTIFIER '(' ')' # primaryExpressionWithCall
	| IDENTIFIER '(' expression
	(
		',' expression
	)* ')' # primaryExpressionWithCall
	| IDENTIFIER '++' # primaryExpressionWithIdentifier
	| IDENTIFIER '--' # primaryExpressionWithIdentifier
	| IDENTIFIER '[' expression ']' # primaryExpressionWithIdentifier
;

unaryExpression
:
	primaryExpression
	| '-' unaryExpression
	| '!' unaryExpression
;

binaryExpression
:
	unaryExpression
	| binaryExpression '*' binaryExpression
	| binaryExpression '+' binaryExpression
	| binaryExpression '-' binaryExpression
	| binaryExpression '<' binaryExpression
	| binaryExpression '>' binaryExpression
	| binaryExpression '<=' binaryExpression
	| binaryExpression '>=' binaryExpression
	| binaryExpression '==' binaryExpression
	| binaryExpression '!=' binaryExpression
;

expression
:
	IDENTIFIER '=' binaryExpression # expressionWithAssignment
	| IDENTIFIER '[' expression ']' '=' binaryExpression #
	expressionWithAssignment
	| binaryExpression # expressionWithoutAssignment
;

declaration
:
	typeName declarator
	(
		',' declarator
	)* ';'
;

typeName
:
	'void'
	| 'int'
	| 'float'
;

declarator
:
	IDENTIFIER
	| '*' IDENTIFIER
	| IDENTIFIER '[' ICONSTANT ']'
	| declarator '(' parameterDeclaration
	(
		',' parameterDeclaration
	)* ')'
	| declarator '(' ')'
;

parameterDeclaration
:
	typeName declarator
;

statement
:
	compoundStatement
	| expression? ';'
	| selectionStatement
	| iterationStatement
	| parallelIterationStatement
	| jumpStatement
;

compoundStatement
:
	'{' declaration* statement* '}'
;

selectionStatement
:
	'if' '(' expression ')' statement
	(
		'else' statement
	)?
;

iterationStatement
:
	'while' '(' condition = expression ')' statement
	| 'for' '(' init = expression? ';' condition = expression? ';' next =
	expression? ')' statement
;

parallelIterationStatement
:
	'#pragma' 'omp' 'parallel' 'for' 'for' '(' i1 = IDENTIFIER '=' from =
	expression ';' i2 = IDENTIFIER '<' to = expression ';' i3 = IDENTIFIER '++'
	')' statement
;

jumpStatement
:
	'return' expression? ';'
;

program
:
	(
		functionDefinition
		| declaration
	)* EOF
;

functionDefinition
:
	returnType = typeName declarator body = compoundStatement
;
