# Parser

EBNF

```
json = object | array;
object = '{' {ws} '}' | '{' members '}';
members = member | member ',' members;
member = {ws} string {ws} ':' element;
array = '[' {ws} ']' | '[' elements ']';
elements = element | element ',' element;
element = {ws} value {ws};
value = object | array | string | number | 'true' | 'false' | 'null';

string = '"' characters '"';
characters = "" | chacater characters;
character = unescaped | escaped
unescaped = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' |
            'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' |
            'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
            'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
            'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' |
            'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' |;
escaped = escape ['"' | '\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' ]
escape = '\'
number = {'-'} digits;
digits = digit | digit1-9 digit;
digit = '0' | digit1-9
digit1-9 = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
```
