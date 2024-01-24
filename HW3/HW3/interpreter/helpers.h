#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int yylex();
int yyerror(char *error);

typedef struct
{
    int numerator;
    int denominator;
}Fraction;

