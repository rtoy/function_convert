# function_convert

Maxima Common Lisp code for applying function identities.

`function_convert` is a Maxima package for performing semantic function-to-function conversions on Maxima expressions.

A conversion rule has the form `f => g`, where `f` is  the source function and `g` is either a target function name
 or a lambda expression.

The operator `=>` indicates a *semantic* conversion, not a literal renaming.  For example, the 
 `sinc => sin` does not replace the symbol `sinc` by `sin`.  Instead, it applies the
identity `sinc(x) = sin(x)/x` so that occurrences `sinc(x)` are rewritten as `sin(x)/x`.

The package makes it easy to expand special functions, replace function calls with equivalent expressions, or define custom transformations without using pattern matching.

## Features

- Convert function calls using built-in identities
- Support user-defined conversions via lambda expressions
- Apply multiple conversions left-to-right
- Simple, readable syntax using `=>`
- Extensible without modifying Maxima internals or using Common Lisp

## Basic Usage

### Semantic conversion using a built-in identity
```maxima 
(%i8)	function_convert(1+sinc(x), 'sinc => 'sin);
(%o8)	sin(x)/x+1

(%i9)	function_convert( (x!)!, "!" => 'gamma);
	
(%9)	gamma(gamma(x+1)+1)
```

### Explicit conversion using a lambda expression
```maxima
(%i9)	function_convert(1+sinc(x), 'sinc => lambda([q], sin(q)/q));
(%o9)	sin(x)/x+1
```

### Chaining multiple conversions

```maxima
(%i12)	function_convert(sinc(x), sinc => sin, sin => exp);
	
(%o12)	-((%i*(%e^(%i*x)-%e^(-(%i*x))))/(2*x))
```

## Syntax

A conversion rule has the form `f => g` where:

- `f` is the source function
- `g` is either a target function name (using a built-in identity) or a lambda expression of one argument

The operator `=>` indicates a semantic conversion, not a literal renaming.

## Error Checking

`function_convert` validates each rule and signals an error for malformed conversions, such as missing arguments or invalid lambda expressions.

## Extensibility

Users may define new conversions by supplying a lambda expression using `function_convert(expr, f => lambda([u], some_expression_in_u));`

No modification of Maxima’s simplifier or pattern matcher is required.

## Motivation

Many systems (Maple, Mathematica, SymPy) provide built‑in expansions or rewrite mechanisms, but Maxima uses an alphabet soup of functions that perform semantic function‑to‑function conversions; examples include `makefac` and `makegamma`. In other cases, transformations are controlled by option variables—for example, `expintrep`. These names are easy to forget and not always easy to locate in the user documentation.

Possibly, `function_convert` will provide a simple, uniform, and user-extensible way to do such conversions.


## Installation

Place the package file in a directory on Maxima’s search path and load it with `load("function_convert");`




