%{
#include "helpers.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "parser.tab.h"

const char* tokenNames[] = {
    	"KW_AND",
	"KW_OR",
	"KW_NOT",
	"KW_EQUAL",
	"KW_LESS",
	"KW_NIL",
	"KW_LIST",
	"KW_APPEND",
	"KW_CONCAT",
	"KW_SET",
	"KW_DEF",
	"KW_FOR",
	"KW_IF",
	"KW_EXIT",
	"KW_LOAD",
	"KW_DISPLAY",
	"KW_TRUE",
	"KW_FALSE",
	"OP_PLUS",
	"OP_MINUS",
	"OP_MULT",
	"OP_DIV",
	"OP_OP",
	"OP_CP",
	"OP_COMMA",
	"COMMENT",
	"VALUEF",
	"IDENTIFIER",
	"SYNTAX_ERROR"
    /* Add more token names as needed */

};
struct Fraction {
    int numerator;
    int denominator;
};
%}

%%
"and"	{ yylval.operand = KW_AND; return KW_AND;}
"or"	{ yylval.operand = KW_OR; return KW_OR;}
"not"	{ yylval.operand = KW_NOT; return KW_NOT;}
"equal"	{ yylval.operand = KW_EQUAL; return KW_EQUAL;}
"less"	{ yylval.operand = KW_LESS; return KW_LESS;}
"nil"	{ yylval.operand = KW_NIL; return KW_NIL;}
"list"	{ yylval.operand = KW_LIST; return KW_LIST;}
"append"	{ yylval.operand = KW_APPEND; return KW_APPEND;}
"concat"	{ yylval.operand = KW_CONCAT; return KW_CONCAT;}
"set"	{ yylval.operand = KW_SET; return KW_SET;}
"def"	{ yylval.operand = KW_DEF; return KW_DEF;}
"for"	{ yylval.operand = KW_FOR; return KW_FOR;}
"if"	{ yylval.operand = KW_IF; return KW_IF;}
"exit"	{ yylval.operand = KW_EXIT; return KW_EXIT;}
"load"	{ yylval.operand = KW_LOAD; return KW_LOAD;}
"display"	{ yylval.operand = KW_DISPLAY; return KW_DISPLAY;}
"true"	{ yylval.operand = KW_TRUE; return KW_TRUE;}
"false"	{ yylval.operand = KW_FALSE; return KW_FALSE;}
"+"	{ yylval.operand = OP_PLUS; return OP_PLUS;}
"-"	{ yylval.operand = OP_MINUS; return OP_MINUS;}
"*"	{ yylval.operand = OP_MULT; return OP_MULT;}
"/"	{ yylval.operand = OP_DIV; return OP_DIV;}
"("	{ yylval.operand = OP_OP; return OP_OP;}
")"	{ yylval.operand = OP_CP; return OP_CP;}
","	{ yylval.operand = OP_COMMA; return OP_COMMA;}
";;".*	{ yylval.operand = COMMENT; return COMMENT;}

[0-9]+"b"[1-9]+		{
			printf("VALUEF %s\n", yytext);
			/* char *token1;
			char *token2;
			
			token1 = strtok(yytext, "b");
			token2 = strtok(NULL, "b");
			Fraction frac_lex;
			frac_lex.numerator = atoi(token1);
			frac_lex.denominator = atoi(token2);
			yylval.frac = frac_lex; */

			strcpy(yylval.expr, yytext);
			return VALUEF;
			}

[0-9]+[_a-zA-Z]+[0-9]*	{ printf("SYNTAX_ERROR %s cannot be tokenized!  \n", yytext); yylval.operand = SYNTAX_ERROR ;return SYNTAX_ERROR;}

[a-zA-Z][_a-zA-Z0-9]*	{ printf("IDENTIFIER\n"); strcpy(yylval.expr, yytext) ;return IDENTIFIER;}

[ \t\n]* ;

.           { printf("SYNTAX_ERROR %s cannot be tokenized!  \n", yytext); yylval.operand = SYNTAX_ERROR ;return SYNTAX_ERROR; }
%%

int yywrap(){}