# NAME
lang - Paren language specification.

# DESCRIPTION
This manual is primarily intended for language implementers.

Language users are advised to refer to `paren-tutorial(7)`.

This document describes the basic concepts of the language and the knowledge required to parse the file `$paren-home/modoles/core.p`, which is loaded at startup.

Therefore, the reader macro defined in the evaluation of the startup file are not included in the description. The extensibility of the language, by the language itself, is one of the most important features of Paren.

## Paren
Paren is a programming languages that written by S-expression and evaluates S-expressions.

## S-expressions
S-expressions is a list or atom.

### Lists
A cons is a data structure with two components called car and cdr.

All cdrs refer to `nil` or cons.

A list is the entire cons that can be traced from a cons.

The entire car is called a list element.

A list without elements is called an empty list and is represented by symbol `nil`.

### Atom
An atom is the following data types.

- symbol
- keyword
- array
- dictionary
- bytes
- function
- macro
- number
- string

#### Symbols
A symbol is a data type for holding a reference to an S-expression.

A symbol is an object representing a string, the symbol's name.

Unlike strings, two symbols whose names are spelled the same way are never distinguishable.

#### Keywords
Keywords are the same as symbols, except that they cannot hold references to other S-expression.

#### Arrays
An array is a data type in which any S-expression is placed in continuous memory.

#### Dictionary
A dictionary is an data type composed of a collection of key-value pairs, such that each possible key appears at most once in the collection.

Operations associated with this data type allow.

- the addition of a pair to the collection;
- the modification of an existing pair;
- the lookup of a value associated with a particular key.

Unlike general dictionaries, pairs cannot be deleted. This limitation is because the main purpose of dictionary is to build the Paren Object System.

Only symbols or keywords can be specified for key.

Any value can be entered in value.

#### Bytes
A bytes is an array that specializes in handling only numbers from `0` to `255`.

#### Functions
A function is a data type that receives an argument of zero or more and evaluates an S-expression of zero or more.

#### Macros
A macro is a data type that has a mechanism to expand its arguments into another S-expression.

#### Numbers
A number is a data type that represents a number.

#### Strings
A string is a data type that represents a string.

## Meta language
The grammar is defined by the following EBNF notation.

    x? -- x can be omitted.
    x | y -- x or y.
    x+ -- One or more repetition of x.
    x* -- Zero or more repetition of x.
    () -- Grouping.
    'x' -- Fixed phrase. Indicates the character sequence. x may be plural.
    [...] -- Character group. One of the characters specified in []. When writen as x-y, one of the ASCII character sets between x and y. The leading ~ indicates any character other than those specified in [].
    = -- Definition. The syntax element indicated on the left side is defined on the right side.

If '\' is described in a fixed phrase or a character group, it means the following character itself.

Exceptions are `\t` for tab characters, `\n` for newline characters.

## Reading rules
### Lexical rules
The lexical rules determines how a character sequence is split into a sequence of lexemes.

This rule is the minimum rule required to read core.p. This is because paren can overwrite the reader by paren itself.

    lexeme = symbol | keyword | string | number | '(' | ')' | '\''
    comment = ';' [~\n]*
    space = [\t\n ]
    symbol = identifier
    keyword = ':' identifier
    string = '"' ([~"\\] | escape-sequence)* '"'
    number = sign? (integer | float)
    identifier = symbol-alpha identifier-rest*
                 | sign ((symbol-alpha | sign) identifier-rest* )?
    identifier-rest = symbol-alpha | digit | sign
    escape-sequence = '\\' ([~cx] | 'c' [@-_a-z] | 'x' hexDigit hexDigit)
    symbol-alpha = [!#$%&*./<=>?A-Z[\]^_a-z{|}~]
    sign = '+' | '-'
    integer = (digit+ 'x')? [0-9a-z]+
    float = digit+ '.' digit+ ('E' [+-]? digit+)?
    hexDigit = digit | [a-f]
    digit = [0-9]
    sign = [+\-]

### Syntax rules
The syntax rules describes the syntax of syntactic datain terms of a sequence of lexemes.

It is possible to write a comment or a space arbitrarily at a break point in the syntax rules.

    s-expression = list | atom
    list = '(' s-expression* ')' | abbrev-list
    abbrev-list = abbrev-prefix s-expression
    abbrev-prefix = '\''
    atom = symbol | keyword | number | string

## Evaluation rules
If the S-expression is one of the following, it is considered evaluable.

- Atom
- List and the first element is one of the following
    - Function
    - Macro
    - Special operator

### Atom
If it is not a symbol, it returns itself.

Otherwise, it returns a value that is resolved from the current environment.

### List
See the manual for implementation specifications of special operators and some of the built in functions.

#### Special operator
Perform a special evaluation for each individual operator.

#### Function
Apply it to the function after evaluating every second and subsequent element of the list.

Built-in functions follow the same rules.

### Macro
Evaluate the resulting S-expressions after applying the second and subsequent elements to the macro.

# SEE ALSO
- data-types(7)
- object-system(7)
- paren-tutorial(7)
