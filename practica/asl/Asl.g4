//////////////////////////////////////////////////////////////////////
//
//    Asl - Another simple language (grammar)
//
//    Copyright (C) 2020-2030  Universitat Politecnica de Catalunya
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU General Public License
//    as published by the Free Software Foundation; either version 3
//    of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Affero General Public License for more details.
//
//    You should have received a copy of the GNU Affero General Public
//    License along with this library; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
//
//    contact: José Miguel Rivero (rivero@cs.upc.edu)
//             Computer Science Department
//             Universitat Politecnica de Catalunya
//             despatx Omega.110 - Campus Nord UPC
//             08034 Barcelona.  SPAIN
//
//////////////////////////////////////////////////////////////////////

grammar Asl;

//////////////////////////////////////////////////
/// Parser Rules
//////////////////////////////////////////////////

// A program is a list of functions
program : function+ EOF
        ;

// A function has a name, a list of parameters and a list of statements
function
        : FUNC ID '(' (parameter_decl)? ')' (COLON basic_type)? (variable_decl)* statements ENDFUNC
        ;

variable_decl
        : VAR ID (COMMA ID)* ':' type
        ;

parameter_decl
        : ID COLON type (COMMA ID COLON type)*
        ;

parameters
        : (expr (COMMA expr)*)?
        ;

type    : basic_type
        | ARRAY LBRACKET INTVAL RBRACKET OF basic_type
        ;

basic_type
        : INT
        | FLOAT
        | BOOL
        | CHAR
        ;

statements
        : (statement)*
        ;

// The different types of instructions
statement
          // Assignment
        : left_expr ASSIGN expr ';'                                 # assignStmt
          // if-then-else statement (else is optional)
        | IF expr THEN statements (ELSE statements)? ENDIF          # ifStmt
          // while statement
        | WHILE expr DO statements ENDWHILE                         # whileStmt
          // return statement
        | RETURN expr? SEMICOLON                                    # returnStmt
          // A function/procedure call has a list of arguments
          // in parenthesis (possibly empty)
        | ident '(' parameters ')' ';'                              # procCall
          // Read a variable
        | READ left_expr ';'                                        # readStmt
          // Write an expression
        | WRITE expr ';'                                            # writeExpr
          // Write a string
        | WRITE STRING ';'                                          # writeString
        ;

// Grammar for left expressions (l-values in C++)
left_expr
        : ident                                         # left_exprIdent
        | ident LBRACKET expr RBRACKET                  # left_exprArray
        ;

// Grammar for expressions with boolean, relational and aritmetic operators
expr    : LPAR expr RPAR                                # parenthesis
        | op=(PLUS|MINUS) expr                          # sign
        | op=NOT expr                                   # not
        | expr op=(MUL|DIV|MOD) expr                    # arithmetic
        | expr op=(PLUS|MINUS) expr                     # arithmetic
        | expr op=(EQ|NE|LT|GT|LE|GE) expr              # relational
        | expr op=AND expr                              # boolean
        | expr op=OR expr                               # boolean
        | ident LBRACKET expr RBRACKET                  # exprArray
        | ident LPAR parameters RPAR                    # exprFunc
        | value                                         # exprValue
        | ident                                         # exprIdent
        ;

// Values
value   : INTVAL
        | FLOATVAL
        | BOOLVAL
        | CHARVAL
        ;

// Identifiers
ident   : ID
        ;

//////////////////////////////////////////////////
/// Lexer Rules
//////////////////////////////////////////////////

LPAR        : '(';
RPAR        : ')';
LBRACKET    : '[';
RBRACKET    : ']';
COMMA       : ',';
COLON       : ':';
SEMICOLON   : ';';
ASSIGN      : '=' ;
EQ          : '==' ;
NE          : '!=' ;
LT          : '<' ;
GT          : '>' ;
LE          : '<=' ;
GE          : '>=' ;
PLUS        : '+' ;
MINUS       : '-';
MUL         : '*';
DIV         : '/';
MOD         : '%';
VAR         : 'var';
INT         : 'int';
FLOAT       : 'float';
BOOL        : 'bool';
CHAR        : 'char';
ARRAY       : 'array';
OF          : 'of';
NOT         : 'not';
AND         : 'and';
OR          : 'or';
IF          : 'if' ;
THEN        : 'then' ;
ELSE        : 'else' ;
ENDIF       : 'endif' ;
WHILE       : 'while';
DO          : 'do';
ENDWHILE    : 'endwhile';
RETURN      : 'return';
FUNC        : 'func' ;
ENDFUNC     : 'endfunc' ;
READ        : 'read' ;
WRITE       : 'write' ;
INTVAL      : ('0'..'9')+ ;
FLOATVAL    : ('0'|('1'..'9') ('0'..'9')*) '.' INTVAL;
BOOLVAL     : ('true'|'false');
CHARVAL     : '\'' ('a'..'z'|'A'..'Z'|'0'..'9'|'_'|'@'|'\''|'\\n'|'\\t'|'\\\''|'.'|':'|';'|' ') '\'';
ID          : ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'_'|'0'..'9')* ;


// Strings (in quotes) with escape sequences
STRING    : '"' ( ESC_SEQ | ~('\\'|'"') )* '"' ;

fragment
ESC_SEQ   : '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\') ;

// Comments (inline C++-style)
COMMENT   : '//' ~('\n'|'\r')* '\r'? '\n' -> skip ;

// White spaces
WS        : (' '|'\t'|'\r'|'\n')+ -> skip ;
// Alternative description
// WS        : [ \t\r\n]+ -> skip ;
