# Maxima package ``function_convert`

The file `function_convert` contains a Maxima package for performing semantic function-to-function conversions on Maxima expressions.

A conversion rule has the form `f = g`, where `f` is the source function and `g` is either a target function or a lambda expression. The rule `f = g` instructs Maxima to replace calls to `f` with calls to `g`. This use of "=" is similar to its use in the Maxima function `substitute`, with one difference: here the operator `=` indicates a *semantic* conversion, not a literal renaming.

For example, the rule `sinc = sin` does **not** replace the symbol `sinc` with `sin`. Instead, it applies the identity `sinc(x) = sin(x)/x`, so occurrences of `sinc(x)` are rewritten as `sin(x)/x`.

The package provides a simple, declarative way to convert between related functions and to define custom transformations without relying on pattern matching.

## Features

- Convert Maxima expressions using built-in identities
- Support user-defined conversions via lambda expressions
- Apply multiple conversions left-to-right
- Simple, readable syntax for conversions using `=`
- Extensible without modifying Maxima internals or using Common Lisp

## Basic Usage

### Semantic conversion using a built-in identity
```maxima 
(%i1) function_convert('sinc = 'sin,1+sinc(x));
(%o1) sin(x)/x+1

(%i2) function_convert("!" = 'gamma, (x!)!);
(%o2) gamma(gamma(x+1)+1)
```

### Explicit conversion using a lambda expression
```maxima
(%i3) function_convert('sinc = lambda([q], sin(q)/q),1+sinc(x));
(%o3) sin(x)/x+1
```

### Chaining multiple conversions
To apply two or more conversions, put the converters into a list; for example
```maxima
(%i4) function_convert(['sinc = 'sin, 'sin = 'exp], sinc(x)^2);
(%o4) -((%e^(%i*x)-%e^-(%i*x))^2/(4*x^2))
```

### Listing all built-in conversions

To print all the built-in converter functions, use `list_converters`. In addition to returning a list
of all converters, the function prints a list of them along with a short description of the converter:
```maxima
(%i5) list_converters();
sinc = sin : Convert sinc(x) into sin(x)/x.
csc = sin : Convert csc(x) into 1/sin(x).
binomial = factorial : Convert binomial(n,k) to factorial form.
log10 = log : Convert log10(x) into log(x)/log(10).

(%o3) [sinc = sin, csc = sin, binomial = factorial, log10 = log, ...]
...
```
If a package defines new conversions, these conversions will be listed once the package is loaded.

To list just the converters with given source functions, list the source functions as arguments:
```maxima
(%i6) list_converters("!", 'tan);

tan = sin : Convert tan(x) into sin(x)/cos(x).
factorial = product : Convert n! to product(g,g,1,n).
factorial = gamma : Convert x! into gamma(1+x).

(%o6) [tan = sin,factorial = product,factorial = gamma]
```
## Syntax

A conversion rule has the form `f = g` where:

- `f` is the source function
- `g` is either a target function (using a built-in identity) or a lambda expression 

The operator `=` indicates a semantic conversion, not a literal renaming. The operator `=` has no evaluation or simplification rule of its own; it is purely notational.

## Error Checking

The function `function_convert` validates each rule and signals an error for malformed conversions, such as missing arguments or invalid lambda expressions.

## Extensibility

Users may define new conversions by supplying a lambda expression using `function_convert(f = lambda([u], some_expression_in_u), expr);`

No modification of Maxima’s simplifier or pattern matcher is required.

Users who have some understanding of Common Lisp and Maxima internals should
be able to define new built-in conversions. The file `function_convert` has some examples; here
is the definition of the converter for `sinc = sin`
```lisp
(define-function-converter (%sinc %sin) (op x)
  "Convert sinc(x) into sin(x)/x."
  (declare (ignore op))
  (let ((z (car x)))
    (div (ftake '%sin z) z)))
```
The function `list_converters` prints the docstring for each converter along with
the identifier for the rule (`f = g`), so it is useful to include a docstring for
each converter function.

It is possible to define a rule that applies to a class of functions, for example to all
trigonmetric functions. Here is the definition of a rule that converts all six trigonometric
functions to exponential form:
```lisp
(define-function-converter (:trig $exp) (op x)
 "Convert all trigonometric functions to exponential form."
  ($exponentialize (fapply op x)))
```
Unlike the `sinc` to `sin` rule, this rule uses the argument `op`.



## Algorithm and Implementation 

The function `function_convert` walks an expression tree and replaces function calls according to well‑defined rules. In that sense, it is straightforward code of a kind repeated many times in Maxima. It is not a pattern matcher or a general rewrite engine.

The package is implemented in Common Lisp. It has been tested using Clozure Common Lisp version 1.13 and SBCL version 2.4.7.

## Limitations & Bugs

The source function can not be subscripted, for example `li[2](x)`. 

## Motivation

Many systems (Maple, Mathematica, SymPy) provide built‑in expansions or rewrite mechanisms, but Maxima uses an alphabet soup of functions that perform semantic function‑to‑function conversions; examples include `makefact` and `makegamma`. In other cases, transformations are controlled by option variables—for example, `expintrep`. 

These names are easy to forget and are not always easy to locate in the user documentation. The function_convert package may offer a simple, uniform, and user‑extensible way to perform such conversions.

## Installation

Place the package file in a directory on Maxima’s search path and load it with `load("function_convert");`

## Using `defrule` as an alternative to `function_convert`

Maxima's pattern-base `defrule` tool is an alternative to using `function_convert`. A simple
example is
```maxima
(%i1) matchdeclare (aa, true)$

(%i2) defrule(sinc_rule,  sinc(aa),sin(aa)/aa)$

(%i3) apply1(sinc(sinc(x)), sinc_rule);
                                       sin(x)
                                 x sin(──────)
                                         x
(%o3)                            ─────────────
                                    sin(x)

```
This method works well, especially for single use function-to-function conversions. But a `kill(all)` removes all rules defined by `defrule` and it still relies on an alphabet soup of functions.

Finally, the package has at least one built-in rule that is difficult to fully duplicate using `defrule`: 

```lisp
(%i5) function_convert('gamma = 'sin, 20252*x*gamma(x)*gamma(1-x) < %pi);
                               20252 %pi x
(%o5)                          ─────────── < %pi
                               sin(%pi x)

```

## History

Initially, the aim of this project was to add a `sinc` function to Maxima. That led to the question of converting expressions from `sinc` form to trigonometric form. I realized that with only a bit more work, the conversion utility could be made much more broadly useful.

For historical reasons only, the original `sinc` package still resides in this repository.

## To Do

- [x] build a self-documenting feature
- [x] re-examine the code that converts to the internal name of an operator
- [x] build a library of useful core rules (a good start)
- [ ] regression tests for `function_convert`
- [x] texinfo documentation for `function_convert` (at least a good start)
- [x] decide if the converter(s) are first `function_convert(f = g,expr)` or last `function_convert(expr, f = g)`
- [x] texinfo documentation for `sinc`
- [x] regression tests for `sinc`
- [x] TeX support for `sinc`
- [x] Update README.md to reflect the change from "=" to "=".








