/* Should be illegal, I think */
`define DEFINE define
``define foo bar

/* Should also be illegal, but accepted by Vivado */
`define amacro(m) `m
`amacro(define) foo bar

/* Not forbidden by anything in the spec? */
`define NAME name
`define `NAME(a) foo

/* Legal, but parameter definition should not be part of the macro def */
`define BACK \/* Whitespace */
`define foo `BACK
parameter int foo = 0

/* Legal? */
`define ARGS (a,b)
`define foo`ARGS a + b

/* Legal? */
`define foo'h1
