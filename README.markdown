# Simple calculator

This is a very simple language I made while learning how to make compilers.

It interprets simple mathematical expressions like:

```
a = 2.
b = 10.
a * ( b + a ) / 4.
```

## Rules of the language

- All statements most end with a period.
- The last line is the result and will be printed.
- Variables are welcome.
- Assignment is an expression. It returns the value being assigned.
- Whitespace is ignored.
- Only supports integer division. There are no floats.

## Running the code

`bin/calc <filename>`

## Compiling the interpreter

1. Install Haskell
2. `ghc -o bin/calc Calc.hs`
