# function_convert


The file `function_convert` is a Maxima package for performing semantic function-to-function conversions on Maxima expressions.

A conversion rule has the form `f => g`, where `f` is the source function and `g` is either a target function 
 or a lambda expression.

The operator `=>` indicates a *semantic* conversion, not a literal renaming.  For example, the rule
 `sinc => sin` does not replace the symbol `sinc` by `sin`.  Instead, it applies the
identity `sinc(x) = sin(x)/x` so that occurrences `sinc(x)` are rewritten as `sin(x)/x`.

The package makes it easy to convert between equivalent special functions and to define custom transformations without using pattern matching.

## Features

- Convert Maxima expressions using built-in identities
- Support user-defined conversions via lambda expressions
- Apply multiple conversions left-to-right
- Simple, readable syntax for conversions using `=>`
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

### Listing all built-in converions

To print all the built-in converter fuctions, use `list_converters`
```maxima
(%i34) list_converters();
sinc => sin : Convert sinc(x) into sin(x)/x.
csc => sin : Convert csc(x) into 1/sin(x).
cos => exp : Convert cos(x) to exponential form.
tan => sin : Convert tan(x) into sin(x)/cos(x).
sin => exp : Convert sin(x) to exponential form.
factorial => gamma : Convert x! into gamma(1+x).
```
If a package defines new conversions, these conversions will be listed once the package is loaded.
## Syntax

A conversion rule has the form `f => g` where:

- `f` is the source function
- `g` is either a target function (using a built-in identity) or a lambda expression of one argument

The operator `=>` indicates a semantic conversion, not a literal renaming. The operator `=>` has no evaluation or simplification rule of its own; it is purely notational.

## Error Checking

The function `function_convert` validates each rule and signals an error for malformed conversions, such as missing arguments or invalid lambda expressions.

## Extensibility

Users may define new conversions by supplying a lambda expression using `function_convert(expr, f => lambda([u], some_expression_in_u));`

No modification of Maxima’s simplifier or pattern matcher is required.

## Algorithm and Implementation 

The function `function_convert` walks an expression tree and replaces function calls according to well-defined rules. As such, it is simple code that is repeated in Maxima hundreds of times. It isn’t a pattern matcher or a general rewrite engine. 

## Limitations & Bugs

For a rule to work correctly, the source function must be the name of a simplifying Maxima function. It 
would be useful to allow the source function to be a collection, say `trig` that matches all trigonometric 
functions, but the code doesn't allow this. Fixing this would require allowing the source function to be a predicate-it's doable.

At the top-level of `convert_function,` there is a bit of code that converts to the internal name of an operator. I suspect that this code has some limitations or bugs.

## Motivation

Many systems (Maple, Mathematica, SymPy) provide built‑in expansions or rewrite mechanisms, but Maxima uses an alphabet soup of functions that perform semantic function‑to‑function conversions; examples include `makefac` and `makegamma`. In other cases, transformations are controlled by option variables—for example, `expintrep`. 

These names are easy to forget and are not always easy to locate in the user documentation. Possibly, `function_convert` will provide a simple, uniform, and user-extensible way to do such conversions.

## Installation

Place the package file in a directory on Maxima’s search path and load it with `load("function_convert");`

## History

Initially, the aim of this project was to add a `sinc` function to Maxima. That led to the question of converting expressions from `sinc` form to trigonometric form. I realized that with only a bit more work, the conversion utility could be made much more broadly useful.

For historical reasons only, the original `sinc` package still resides in this repository.

## To Do

- [x] build a self-documenting feature
- [ ] re-examine the code that converts to the internal name of an operator
- [ ] build a library of useful core rules
- [ ] regression tests
- [ ] documentation








