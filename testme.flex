/* -*- Mode: Flex -*- */

%{
#include <stdio.h>
%}

V  [aeiou]
C  [bcdfghjklmnpqrstvwxz]

%%

"#b"      { puts("b"); }
"b"/"#"   { puts("p"); }
"b"/{V}   { puts("b"); }
"b"       { puts("b"); }

. { printf("(unmatched:%c)", yytext[0]); } /* ignore unmatched input */

%%
int main(void)
{
  yylex();
  return 0;
}

int yywrap(void) { return 1; }
