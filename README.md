# Semantic-Analyzer

This is a program to evaluate the type of a simply typed lambda calculus expression defined by the following grammar:

```bnf
<expressão>       ::= <termo> | <expressão>

<termo>           ::= true
                    | false
                    | if <termo> then <termo> else <termo> endif
                    | <número>
                    | suc
                    | pred
                    | ehzero
                    | <variável>
                    | ( <termo> <termo> )
                    | lambda <variável> : <tipo> . <termo> end

<número>          ::= <dígito> | <dígito-não-zero> <seq-dígitos>
<dígito>          ::=        0 | <dígito-não-zero>
<dígito-não-zero> ::=        1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<seq-dígitos>     ::= <dígito> | <dígito> <seq-dígitos>

<variável>    (*) ::= <letra> | <letra> <seq-alfa-num>

<letra>           ::= a | b | c | d | e | f | g | h | i
                    | j | k | l | m | n | o | p | q | r
                    | s | t | u | v | w | x | y | z
                    | A | B | C | D | E | F | G | H | I
                    | J | K | L | M | N | O | P | Q | R
                    | S | T | U | V | W | X | Y | Z

<seq-alfa-num>   ::= <alfa-num> | <alfa-num> <seq-alfa-num>

<alfa-num>       ::= <letra> | <dígito>

<tipo>           ::= Bool | Nat | ( <tipo> -> <tipo> )
```

Example:

```lambda x : Nat . x end``` is an expression for the identity function whose type is ( Nat -> Nat )
and ```( lambda x : Nat . x end 1 )``` reduces to Nat
