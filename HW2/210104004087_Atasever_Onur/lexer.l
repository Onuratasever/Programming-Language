%{
#include <stdio.h>
%}

%%
"and"	{ printf("KW_AND\n"); }
"or"	{ printf("KW_OR\n"); }
"not"	{ printf("KW_NOT\n"); }
"equal"	{ printf("KW_EQUAL\n"); }
"less"	{ printf("KW_LESS\n"); }
"nil"	{ printf("KW_NIL\n"); }
"list"	{ printf("KW_LIST\n"); }
"append"	{ printf("KW_APPEND\n"); }
"concat"	{ printf("KW_CONCAT\n"); }
"set"	{ printf("KW_SET\n"); }
"def"	{ printf("KW_DEF\n"); }
"for"	{ printf("KW_FOR\n"); }
"if"	{ printf("KW_IF\n"); }
"exit"	{ printf("KW_EXIT\n"); }
"load"	{ printf("KW_LOAD\n"); }
"display"	{ printf("KW_DISPLAY\n"); }
"true"	{ printf("KW_TRUE\n"); }
"false"	{ printf("KW_FALSE\n"); }
"+"	{ printf("OP_PLUS\n"); }
"-"	{ printf("OP_MINUS\n"); }
"*"	{ printf("OP_MULT\n"); }
"/"	{ printf("OP_DIV\n"); }
"("	{ printf("OP_OP\n"); }
")"	{ printf("OP_CP\n"); }
","	{ printf("OP_COMMA\n"); }
";;".*	{ printf("COMMENT\n" + 2); }

[1-9]*"b"[0-9]*	{ printf("VALUEF\n"); }

[0-9]+[_a-zA-Z]+[0-9]*	{ printf("SYNTAX_ERROR %s cannot be tokenized!  \n", yytext); }

[a-zA-Z][_a-zA-Z0-9]*	{ printf("IDENTIFIER\n"); }

[ \t\n]* ;

.	{ printf("SYNTAX_ERROR %s cannot be tokenized!  \n", yytext); }

%%
