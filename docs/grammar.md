# Grammar

Syntax described in Backus-Naur Form:

```md
<file> ::= <expression>
<expression> ::= "let " <ident> " = " <abstraction> "; " <expression> | <application>
<application> ::= (<abstraction> | <ident>) "! " <atom>? (" " <atom>)* | <abstraction>
<abstraction> ::= "(" <ident>? (" " <ident>)* ")" " => " <abstraction> | <atom>
<atom> ::= <ident> | <number> | <string> | "(" <expression> ")" | "[" (<expression> (", " <expression>)*)? "]"
<ident> ::= [a-z]+
<number> ::= [0-9]+
<string> ::= "\"" ([a-z] | <number>)* (" " ([a-z] | <number>)*)* "\""
```
