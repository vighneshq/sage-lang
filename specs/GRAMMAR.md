# Sage Grammar Specification

<!-- TODO - Add examples, and details. Maybe move to reST ? -->

## General

**program** &rarr; **stmt_list**

## Statements

**stmt_list** &rarr; { **stmt** } \
**stmt** &rarr; **while_stmt** | **if_stmt** | **jump_stmt** | **return_stmt** | **expr_stmt**

### While Statement

**while_stmt** &rarr; "while" **expr** "{" **stmt_list** "}"

### Jump Statement

**jump_stmt** &rarr; **break_stmt** | **continue_stmt** \
**break_stmt** &rarr; "break" ";" \
**continue_stmt** &rarr; "continue" ";"

### Return Statement

**return_stmt** &rarr; "return" [ **expr** ] ";"


### Expression Statement

**expr_stmt** &rarr; **expr** ";"

## Expressions

**expr** &rarr; **or_expr**

### Logical Expressions

**or_expr** &rarr; **and_expr** { "or" **and_expr** } \
**and_expr** &rarr; **not_expr** { "and" **not_expr** } \
**not_expr** &rarr; "not" **not_expr** | **cond_expr**

### Conditional Expressions

**cond_expr** &rarr; **add_expr** { **cond_op** **add_expr** } \
**cond_op** &rarr; "==" | "!=" | ">" | ">=" | "<" | "<="

### Arithmetic Expressions

**add_expr** &rarr; **mul_expr** { **add_op** **mul_expr** } \
**add_op** &rarr; "+" | "-" | "|" | "^"

**mul_expr** &rarr; **unary_expr** { **mul_op** **unary_expr** }
**mul_op** &rarr; "*" | "/" | "%" | ">>" | "<<"

**unary_expr** &rarr; **unary_op** **unary_expr** | **exp_expr** \
**unary_op** &rarr; "-" | "~"

**exp_expr** &rarr; **call_expr** [ "**" **exp_expr** ]

### Call Expressions

**call_expr** &rarr; **primary_expr** | **function_call_expr**

**function_call_expr** &rarr; **primary_expr** "(" **arg_list** ")" \
**arg_list** &rarr; { **expr** }

### Primary Expressions

**primary_expr** &rarr; **literal_expr** | **variable_expr** | **grouped_expr** \
**literal_expr** &rarr; **literal** \
**variable_expr** &rarr; **identifier** \
**grouped_expr** &rarr; "(" **expr** ")"

## Lexical Grammar

**literal** &rarr; **bool_lit** | **char_lit** | **int_lit** | **real_lit** | **string_lit** \

**bool_lit** &rarr; "true" | "false" \
**char_lit** &rarr; "'" **valid_ascii_char** "'" \
**int_lit** &rarr; "0" | **non_zero_digit** **digit** \* \
**float_lit** &rarr; **int_lit** "." **digit**+ \
**string_lit** &rarr; ' " ' **valid_ascii_char_except_double_quote** ' " '

**identifier** &rarr; **ascii_letter** (**ascii_letter** | **digit** | _)\* [ except reserved words ]

**ascii_letter** &rarr; "a"  ... "z" | "A" ... "Z" \
**digit** &rarr; "0" | **non_zero_digit** \
**non_zero_digit** &rarr; "1" ... "9"

**reserved_words** &rarr; "true" | "false" | "and" | "or" | "not" | "Bool" | "Char" \
"Char" | "Int" | "Real" | "String" | "if" | "else" | \
"while" | "break" | "continue" | "return"

**comments** &rarr; **single_comment** | **multi_comment**

**single_comment** &rarr; "--" **ascii_char_except_newline**\* "\\n"
**multi_comment** &rarr; "!-" (**ascii_char_except_minus** [ "-" **ascii_char_except_excalamation** ])* "-!"
