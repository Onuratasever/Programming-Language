%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "helpers.h"

int yylex();
int yyerror(char* s);
char* createNewString(char* original, char* argument, char* newString);
Fraction createFraction(char *toFraction);

typedef struct FUNC{
        char name[50];
        char implementation[100];
        int numberOfArguments;
        char arguments[4][50];
    }Function;

int gcd(int a, int b) {

    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    if(a != 0) return a;
    return 1;
}

Function* functions;
int numberOfFunc = 0; 

char* FractiontoStr(Fraction toString)
{
    // i need to allocate space for pointer
    char* converted_fraction = calloc(50, sizeof(char));
    if (converted_fraction == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }

    // Convert the fraction to a string
    sprintf(converted_fraction, "%db%d", toString.numerator, toString.denominator);
    /* printf("\nFRACTION TO STR  = %s\n",result); */
    return converted_fraction;
}

int isValuef(char *str)
{
    char *temp = str;
    int i= 0;
    int isOk=0;
        //printf("GElene str: %s\n", str);

    while(temp[i] != '\0')
    {
       // printf("temp yazıyo: %c\n", temp[i]);
        if((temp[i] >= '0' && temp[i] <= '9'))
            isOk = 1;
        if(!(temp[i] >= '0' && temp[i] <= '9') && !(temp[i] == 'b') && !(temp[i] == '-'))
            return 0;
        if(temp[i] == 'b' && isOk == 1)
            return 1;
        i++; //3/4 -1/4
    }
    return 0;
}

int isSmaller(char *valuef1 , char* valuef2) // It calculates difference between two valuef and returns 0 if equal 1 if less -1 if greater
{
    //printf("valuef 1 %s, valuef 2 %s\n", valuef1, valuef2);
    Fraction frac1;
    Fraction frac2;
    Fraction result;
     char *temp = calloc(500,1);
    strcpy(temp,valuef2);
    if(isValuef(valuef1) == 1 && isValuef(temp) == 1)
    {
         frac1 = createFraction (valuef1);
         frac2 =  createFraction (temp);
         result;
        result.numerator = frac1.numerator*frac2.denominator - frac2.numerator*frac1.denominator;
        result.denominator = frac1.denominator * frac2.denominator;
         if(frac1.numerator == 0 && frac1.numerator == 0)
            return 0;
    // printf("num: %d denom: %d\n", result.numerator, result.denominator);
        if(result.numerator < 0)
            return 1;
        if(result.numerator == 0)
            return 0;
        else
            return -1;
    
    }
   
}

Fraction createFraction(char *toFraction) { // It takes a string and it converts it to a fraction
    char *token1;
    char *token2;
    
    token1 = strtok(toFraction, "b");
    token2 = strtok(NULL, "b");
    
    Fraction frac;
    frac.numerator = atoi(token1);
    frac.denominator = atoi(token2);
    return frac;
}

char * sum(char *valuef1, char *valuef2)
{
  //  printf("valuef 1 %s, valuef 2 %s\n", valuef1, valuef2);
    if(isValuef(valuef1) == 1 && isValuef(valuef2) == 1)
    {
        Fraction frac1 = createFraction (valuef1);
        Fraction frac2 =  createFraction (valuef2);
        Fraction result;
        result.numerator = frac1.numerator*frac2.denominator + frac2.numerator*frac1.denominator;
        result.denominator = frac1.denominator * frac2.denominator;
        int gcdval = gcd(result.denominator, result.numerator);
        result.denominator /= gcdval;
        result.numerator /= gcdval;
    // printf("SUM num: %d denom: %d\n", result.numerator, result.denominator);
        return (FractiontoStr (result));
    }
    char *exp = calloc(500,1);
    strcpy(exp,"( + ");;
    strcat(exp,valuef1);
    strcat(exp," ");
    strcat(exp,valuef2);
    strcat(exp,")");
  //  printf("EXP son hali %s\n", exp);

    return exp;
    
	// return createFraction(num, denom);
}

char * sub(char * valuef1, char * valuef2)
{
    //printf("valuef 1 %s, valuef 2 %s\n", valuef1, valuef2);
    if(isValuef(valuef1) == 1 && isValuef(valuef2) == 1)
    {
        Fraction frac1 = createFraction (valuef1);
        Fraction frac2 =  createFraction (valuef2);
        Fraction result;
        result.numerator = frac1.numerator*frac2.denominator - frac2.numerator*frac1.denominator;
	    result.denominator = frac1.denominator * frac2.denominator;
         int gcdval = gcd(result.denominator, result.numerator);
        result.denominator /= gcdval;
        result.numerator /= gcdval;
    // printf("SUB num: %d denom: %d\n", result.numerator, result.denominator);
        return (FractiontoStr (result));
    }
    char *exp = calloc(500,1);
    strcpy(exp,"( - ");;
    strcat(exp,valuef1);
    strcat(exp," ");
    strcat(exp,valuef2);
    strcat(exp,")");
   // printf("EXP son hali %s\n", exp);

    return exp;
}

char * mult(char * valuef1, char * valuef2)
{
   // printf("valuef 1 %s, valuef 2 %s\n", valuef1, valuef2);
   // printf(" %s için sonuç: %d \n %s için sonuç: %d \n",valuef1,isValuef(valuef1),valuef2,isValuef(valuef2));
    if(isValuef(valuef1) == 1 && isValuef(valuef2) == 1)
    {
        Fraction frac1 = createFraction (valuef1);
        Fraction frac2 =  createFraction (valuef2);
        Fraction result;
        result.numerator= frac1.numerator* frac2.numerator;
	    result.denominator = frac1.denominator * frac2.denominator;
         int gcdval = gcd(result.denominator, result.numerator);
        result.denominator /= gcdval;
        result.numerator /= gcdval;
     //printf("MULT num: %d denom: %d\n", result.numerator, result.denominator);
        return (FractiontoStr (result));
    }
    char *exp = calloc(500,1);
    strcpy(exp,"( * ");;
    strcat(exp,valuef1);
    strcat(exp," ");
    strcat(exp,valuef2);
    strcat(exp,")");
   // printf("EXP son hali %s\n", exp);

    return exp;
    
}

char * divv(char *valuef1, char *valuef2)
{
    // printf("valuef 1 %s, valuef 2 %s\n", valuef1, valuef2);
    if(isValuef(valuef1) == 1 && isValuef(valuef2) == 1)
    {
        Fraction frac1 = createFraction (valuef1);
        Fraction frac2 =  createFraction (valuef2);
        Fraction result;
       result.numerator = frac1.numerator* frac2.denominator;
	    result.denominator= frac1.denominator * frac2.numerator;
         int gcdval = gcd(result.denominator, result.numerator);
        result.denominator /= gcdval;
        result.numerator /= gcdval;
    // printf("num: %d denom: %d\n", result.numerator, result.denominator);
        return (FractiontoStr (result));
    }
    char *exp = calloc(500,1);
    strcpy(exp,"( / ");;
    strcat(exp,valuef1);
    strcat(exp," ");
    strcat(exp,valuef2);
    strcat(exp,")");
    //printf("EXP son hali %s\n", exp);

    return exp;
}

Function searchFuncList(char* name) { // It searchs function list
    int i;
    for (i = 0; i < numberOfFunc; i++) {
        if (strcmp(functions[i].name, name) == 0) {
            printf("Found\n");
            return functions[i];
        }
    }
    Function function;
    printf("Not found\n");
    return function;
}
void addFunc(Function addedObj) // It adds functions to function list
{
    Function *temp = calloc(numberOfFunc + 1, sizeof(Function));
    int i;
    if (temp == NULL) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    for (i = 0; i < numberOfFunc; i++) {
        temp[i] = functions[i];
    }

    temp[i] = addedObj;
    numberOfFunc++;
    free(functions);
    functions = temp;
    temp = NULL;
    printf("Function is successfully added!\n");
  //  printf("Num of %d!\n", numberOfFunc);

    /*for(i=0; i <numberOfFunc; i++)
    {
        printf("%d th struct name: %s\n", i, functions[i].name);
    }*/
}
char* subseq(char* string, int position_first, int position_last ) {// It creates a sub string but also ıt adjust white spaces
    char* result;
    result = (char*) malloc(sizeof(char) * (position_last - position_first + 10));
    // strncpy(result, string + position, length);
    int i, j;
    for(i=position_first, j=0; i<=position_last; j++, i++)
    {   
        if(((i == position_first + 1) && string[position_first + 1] != ' ') || ((i == position_last - 1) && string[position_last - 1] != ' '))
        {
            result[j] = string[i];
            j++;
            result[j] = ' ';
           // printf("içeri geliyo is:index: %d |%c|\n",i, string[i]);

        }
        else{
            result[j] = string[i];

        }


            //printf("Result %d is: |%c|\n", j, result[j]);
    }
    result[j] = '\0';  // Null-terminate the result
    return result;
}

char* subString(char* string, int position_first, int position_last ) {
    char* result;
    result = (char*) malloc(sizeof(char) * (position_last - position_first + 10));
    // strncpy(result, string + position, length);
    int i, j;
    for(i=position_first, j=0; i<=position_last; j++, i++)
    {   
        result[j] = string[i];
    }
    result[i] = '\0';  // Null-terminate the result
    return result;
}

char* findResult(char *exp) // It finds inner most expression than it calculates than it changes all string with new result.
{
   // bool ended = false;
    int k = 0;
    int i;
    int counter = 0, j;
    int pos_op, pos_cp;

    char *result = calloc(50,1);
  
        for(i = 0; exp[i] != '\0'; i++) // ( + 4b1 5b1)
        {
           //Fınds first closing paranthesis
            if(exp[i] == ')')
            {
                pos_cp = i;
                // After find first  closing paranthesis it goes back and it finds the indexes of inner most expression
                for(int k = i; k>=0; k--) 
                {
                    if(exp[k] == '(')
                    {
                        pos_op = k;
                        char *expression = subseq(exp, k, i); // It creates a sub string with white spaces
                        char *subb = subString(exp, k, i); // It copies string which is going to be removed
                         //printf("SUB EXP %s\n", expression);
                         //printf("SUBBBBBBB %s\n", subb);

                        char *resOfOp;
                        j = 0;
                        while(counter < 2) // It finds second token except white spaces.
                        {
                            if(expression[j] != ' ')
                                counter++;
                            if(counter != 2)
                                j++;
                        }
                        counter = 0;
                        // According to operations takes tokens and it calls proper operation
                        if(expression[j] == '+')
                        {
                            char *token = strtok(expression, " ");
                            char *token2 = strtok(NULL, " ");
                            char *token3 = strtok(NULL, " ");

                            char *token4 = strtok(NULL, " ");
                            resOfOp = sum(token3, token4);
                            char * yeni;
                             exp = createNewString(exp, subb, resOfOp);
                            i = -1;
                         //printf("EXPRESSİONNNNNNNNNNNNNNNNN %s\n", exp);

                           // printf("tokens %s %s %s %s\n", token, token2, token3, token4);
                        }
                        else if(expression[j] == '-')
                        {
                            char *token = strtok(expression, " ");
                            char *token2 = strtok(NULL, " ");
                            char *token3 = strtok(NULL, " ");
                            char *token4 = strtok(NULL, " ");
                            resOfOp = sub(token3, token4);
                             exp = createNewString(exp, subb, resOfOp);
                            
                            i = -1;
                             //printf("EXPRESSİONNNNNNNNNNNNNNNNN %s\n", exp);

                            //printf("tokens %s %s %s %s\n", token, token2, token3, token4);
                        }
                        else if(expression[j] == '*')
                        {
                            char *token = strtok(expression, " ");
                            char *token2 = strtok(NULL, " ");
                            char *token3 = strtok(NULL, " ");

                            char *token4 = strtok(NULL, " "); // (* (+ x 1b2) (- x 1b2))
                            resOfOp = mult(token3, token4);
                           // printf("multtaki sonuCCCCCCCCCC: %s\n", resOfOp);
                             exp = createNewString(exp, subb, resOfOp);
            
                            i = -1;
                          //printf("EXPRESSİONNNNNNNNNNNNNNNNN %s\n", exp);

                            
                           // printf("tokens %s %s %s %s\n", token, token2, token3, token4);
                        }
                        else if(expression[j] == '/')
                        {
                            char *token = strtok(expression, " ");
                            char *token2 = strtok(NULL, " ");
                            char *token3 = strtok(NULL, " ");

                            char *token4 = strtok(NULL, " ");
                            resOfOp = divv(token3, token4);
                    
                             exp = createNewString(exp, subb, resOfOp);
                           // printf("GELİYOOOOOOOOO \n");
                            i = -1;
                           // printf("EXPRESSİONNNNNNNNNNNNNNNNN %s\n", exp);

                            //printf("tokens %s %s %s %s\n", token, token2, token3, token4);
                        }
                        
                    }
                }
            }
        }
                        // printf("SONHALİ %s\n", exp);
    
    return exp;
}

char* createNewString(char* original, char* argument, char* newString) {
    char* final_version;
    char* ins;    // the next insert point
    char* forTemp;    // temp for values
    int len_argument;  // length of newString (the string to replace)
    int lenOfNewString; // length of string is goin to be replcaed (the string to replace with)
    int len_front; // distance 
    int counter;    // number of newStrings

    // it checks nullity
    if (!original || !argument)
        return NULL;
    len_argument = strlen(argument);
    if (len_argument == 0)
        return NULL; // if arguments len is zero it means there is no string to replace
    if (!newString)
        newString = "";
    lenOfNewString = strlen(newString);

    // counter the number of newStrings needed
    ins = original;
    for (counter = 0; (forTemp = strstr(ins, argument)); ++counter) {
        ins = forTemp + len_argument;
    }
// It allocates places to put new string
    forTemp = final_version = malloc(strlen(original) + (lenOfNewString - len_argument) * counter + 1);

    if (!final_version)
        return NULL;
    while (counter--) {
        ins = strstr(original, argument);
        len_front = ins - original;
        forTemp = strncpy(forTemp, original, len_front) + len_front;
        forTemp = strcpy(forTemp, newString) + lenOfNewString;
        original += len_front + len_argument; 
    }
    strcpy(forTemp, original);
    return final_version;
}

%}

%union{
    int operand;
    int *operands;
    char expr[500];
    Fraction frac;
}

%token KW_AND 
%token KW_OR 
%token KW_NOT 
%token KW_EQUAL 
%token KW_LESS 
%token KW_NIL 
%token KW_LIST 
%token KW_APPEND 
%token KW_CONCAT
%token KW_SET 
%token KW_DEF 
%token KW_FOR 
%token KW_IF 
%token KW_EXIT 
%token KW_LOAD 
%token KW_DISPLAY 
%token KW_TRUE 
%token KW_FALSE
%token OP_PLUS 
%token OP_MINUS 
%token OP_MULT 
%token OP_DIV 
%token <operand> OP_OP 
%token OP_CP 
%token OP_COMMA 
%token COMMENT 
%token <expr> VALUEF 
%token <expr> IDENTIFIER 
%token SYNTAX_ERROR

%type <expr> EXP
%type <expr> FUNCTION


%start begin

%%
begin:
    Start | Start begin ;

Start : EXP { printf("Start -> Exp\n"); printf("EXP: %s\n", $1); }
      | FUNCTION { printf("Start -> Function\n"); }
      |OP_OP KW_EXIT OP_CP { printf("EXIT!\n"); exit(0);}
      ;


EXP : OP_OP OP_PLUS EXP EXP OP_CP { /*printf("Exp -> OP_OP OP_PLUS EXP EXP OP_CP\n");*/ strcpy($$,sum($3, $4)); }
    | OP_OP OP_MINUS EXP EXP OP_CP { /*printf("Exp -> OP_OP OP_MINUS EXP EXP OP_CP\n");*/ strcpy($$,sub($3, $4));} 
    | OP_OP OP_MULT EXP EXP OP_CP { /*printf("Exp -> OP_OP OP_MULT EXP EXP OP_CP\n");*/ strcpy($$,mult($3, $4)); }
    | OP_OP OP_DIV EXP EXP OP_CP { /*printf("Exp -> OP_OP OP_DIV EXP EXP OP_CP\n");*/ strcpy($$,divv($3, $4));}
    | OP_OP IDENTIFIER OP_CP { 
                                    //printf("Exp -> OP_OP IDENTIFIER EXP OP_CP\n"); 
                                    Function found = searchFuncList($2);
                                    // char *newContext = createNewString(found.implementation, found.arguments[0], $3);
                                    //printf("First New context: %s \n", found.implementation);
                                    strcpy($$,  found.implementation);
                                }
    | OP_OP IDENTIFIER EXP OP_CP { 
                                    //printf("Exp -> OP_OP IDENTIFIER EXP OP_CP\n"); 
                                    Function found = searchFuncList($2);
                                    char *newContext = createNewString(found.implementation, found.arguments[0], $3);
                                    printf("Second New context: %s \n", newContext);
                                    strcpy($$,  findResult(newContext));
                                }
    | OP_OP IDENTIFIER EXP EXP OP_CP { 
                                        //printf("Exp -> OP_OP IDENTIFIER EXP EXP OP_CP\n"); 
                                        Function found = searchFuncList($2);
                                        printf("body: %s, argument: %s \n", found.implementation, found.arguments[0]);
                                        char *newContext = createNewString(found.implementation, found.arguments[0], $3);
                                        newContext = createNewString(newContext, found.arguments[1], $4);
                                        //printf("Third New context: %s \n", newContext);
                                        strcpy($$,  findResult(newContext));
                                    }
      | OP_OP KW_IF EXP EXP EXP OP_CP{
                                        //printf("Function -> OP_OP KW_IF EXP EXP EXP OP_CP\n");
                                        if(strcmp($3,"true") == 0)
                                            strcpy($$, $4);
                                        if(strcmp($3,"false") == 0) 
                                            strcpy($$, $5);
                                        }
        | OP_OP KW_LESS EXP EXP OP_CP
        { 
            //printf("Function -> OP_OP KW_LESS EXP EXP EXP OP_CP\n");

            if(isSmaller($3,$4) == 1)
            {
                strcpy($$, "true");
            }
            else
            {
                strcpy($$, "false");
            }
        }
        | OP_OP KW_EQUAL EXP EXP OP_CP
        { 
            //printf("Function -> OP_OP KW_EQUAL EXP EXP EXP OP_CP\n");

            if(isSmaller($3,$4) == 0 )
            {
                strcpy($$, "true");
            }
            else
            {
               strcpy($$, "false");
            }
        }
        | OP_OP KW_AND EXP EXP OP_CP
           {
               if(isSmaller(($3),("0b4")) == 0 || isSmaller(($4),("0b4")) == 0 )
                    strcpy($$, "false");
                else
                    strcpy($$, "true");
           }
          | OP_OP KW_OR EXP EXP OP_CP
           {
               if(isSmaller(($3),("0b4")) != 0 || isSmaller(($4),("0db4")) != 0)
                   strcpy($$, "true");
                else
                   strcpy($$, "false");
           }                          


    | OP_OP IDENTIFIER EXP EXP EXP OP_CP { 
                                            //printf("Exp -> OP_OP IDENTIFIER EXP EXP EXP OP_CP\n"); 
                                        }

    | IDENTIFIER { //printf("Exp -> IDENTIFIER\n"); 
                    strcpy($$, $1); 
                }

    | VALUEF { 
                    //printf("Exp -> VALUEF\n"); 
                strcpy($$, $1); }
    ;

FUNCTION : OP_OP KW_DEF IDENTIFIER EXP OP_CP {
                                                /* printf("Function -> OP_OP KW_DEF IDENTIFIER EXP OP_CP\n" );*/ 
                                                 //printf("IDENTIFIER : %s\n",$3);
                                                 //printf("EXP 1 : %s\n",$4);
                                                Function function;
                                                function.numberOfArguments = 0;
                                                strcpy(function.implementation, $4);
                                                strcpy(function.name, $3);
                                                addFunc(function);
                                                
                                            }
         | OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP {
                                                            // printf("Function -> OP_OP KW_DEF IDENTIFIER IDENTIFIER EXP OP_CP\n"); 
                                                                 Function function;
                                                                function.numberOfArguments = 1;
                                                                strcpy(function.implementation, $5);
                                                                strcpy(function.name, $3);
                                                                strcpy(function.arguments[0], $4);
                                                                addFunc(function);
                                                        }
         | OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP { 
                                                                        //printf("Function -> OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER EXP OP_CP\n"); 
                                                                        Function function; 
                                                                        function.numberOfArguments = 2;
                                                                        strcpy(function.implementation, $6);
                                                                        strcpy(function.name, $3);
                                                                        strcpy(function.arguments[0], $4);
                                                                        strcpy(function.arguments[1], $5);
                                                                        addFunc(function);
                                                                    }
       
         ;

%%

int yyerror(char *error) {
    printf("SYNTAX ERRORrrr \n");
    return 0; 
}

int main() {
    yyparse();
    return 0;
}