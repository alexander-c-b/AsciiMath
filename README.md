# AsciiMath

AsciiMath is a compiler from the [AsciiMath](http://asciimath.org/) language to LaTeX.

Please note: compatability with either the original AsciiMath or [Kerl13/AsciiMath](https://github.com/Kerl13/AsciiMath) is **an explicit non-goal**.  While the original projects are excellent, the standard AsciiMath lacks several features important to my needs, namely a less verbose matrix syntax and a syntax for units (see issue #11).

The aim of this project is most importantly to provide a [Pandoc](http://pandoc.org) filter to use AsciiMath in Markdown documents.

## Install
Use [Nix](http://nixos.org):
-   `nix build` will create a local `result/` directory with the executables.
-   To install to your user environment, do `nix-env -i -f.`.
-   If you have [`direnv`](https://nixos.wiki/wiki/Development_environment_with_nix-shell) set up, run `direnv allow` to setup a development environment.  Otherwise use `nix-shell` at the project root.


## Usage

The executable `asciimath` reads AsciiMath code from its standard input, compile it and prints the resulting LaTeX code on its standard output. For example:

```
> echo "sum_(i=1)^n i^3=((n(n+1))/2)^2" | ./asciimath
\sum _{{i=1}}^{n}i^{3}=\left(\frac{{n\left(n+1\right)}}{2}\right)^{2}
```

or in "interactive mode"

```
> asciimath
sin(2x) = 2sin(x)cos(x) ^D
\sin \left(2x\right)=2\sin \left(x\right)\cos \left(x\right)
```

The executable `pandoc-asciimath` is a [pandoc filter](http://pandoc.org/scripting.html). An example of use would be

```
> pandoc --standalone -t latex --filter pandoc-asciimath file.md -o file.pdf
```

The `Asciimath` module also provide four functions:
-   `readAscii :: String -> Either LexicalError Ast.Code`
-   `writeTeX :: Ast.Code -> String`
-   `compile :: String -> Either LexicalError String`
-   `run :: String -> String`

which can be used in any Haskell program to play with the AST or anything else.

The `run` function do the same work as `compile` but raises a system error if it fails whereas `compile` returns either a `LexicalError` or the expected compiled `String` which is much more convenient for error handling.

## Grammar

This is the grammar used to parse AsciiMath.  It is changed significantly from [Kerl13/AsciiMath](https://github.com/Kerl13/AsciiMath) and from the original project.

```
Code       := Exprs | Matrix
Exprs      := Expr [Exprs]
Matrix     := Exprs '&' Columns | Columns ';;' Rows
Columns    := Exprs ['&' Columns]
Rows       := Columns [';;' Rows]
Exprs      := [Expr SpaceExprs]
SpaceExprs := [Expr SpaceExprs] -- with spaces added before differentials
Expr       := Simple | Simple '/' Simple
Simple     := Term | Unary Term | Binary Term Term
Term       := STerm | STerm '_' STerm | STerm '^' STerm
            | STerm '_' STerm '^' STerm
STerm      := "..." | L Code R | Const

L := '(' | '[' | '{' | '(:' | '{:'

R := ')' | ']' | '}' | ':)' | ':}'

Unary := 'sqrt' | 'text' | 'bb' | 'bbb' | 'cc'  | 'tt'   | 'fr'  | 'sf'
       | 'hat'  | 'bar'  | 'ul' | 'vec' | 'dot' | 'ddot'

Binary := 'frac' | 'root' | 'stackrel'

Const := 'd[a-zA-Z]?' | '[a-zA-Z]+' | numbers | greek letters | ','
       | StandardFunctions | other symbols -- see http://asciimath.org/#syntax

StandardFunctions := 'sin'  | 'cos'  | 'tan' | 'csc' | 'sec' | 'cot' | 'sinh'
                   | 'cosh' | 'tanh' | 'log' | 'ln'  | 'exp' | 'det' | 'dim'
                   | 'lim'  | 'mod'  | 'gcd' | 'lcm' | 'min' | 'max'
```


## Rendering rules

-   The constants are converted to their LaTeX equivalent.
-   `sqrt`, `hat`, `bar`, `vec`, `dot` and `ddot` are prefixed with a ` \ `.
-   `text` and `ul` correspond to the `\text` and `underline` functions.
-   `bb`, `bbb`, `cc`, `tt`, `fr` and `sf` correspond to the `\boldsymbol`, `\mathbb`, `\mathcal`, `\texttt`, `\mathfrak` and `\textsf` functions.
-   `frac` is rendered as a fraction, `root n x` as the `n`-th root of `x` and.
-   `stackrel x y` displays `x` upon `y`.
-   Any text placed between a pair of `"` is rendered in the same font as normal text.
-   `/` stands for a fraction.
-   The `_` and `^` tokens have the same behaviour as in LaTeX but the subscript must be placed before the superscript if they are both present

### Delimiters

Left and right delimiters are preceded by the `\left` and `\right` commands to be well-sized. `(:` and `:)` are chevrons (angle brackets). `{:` and `:}` are invisible delimiters like LaTeX's `{` and `}`. The other delimiters are rendered as expected.

Useless delimiters are automatically converted to invisible braces (`{...}`) in expressions like
-   `(...)/(...)`
-   `..._(...)`, `...^(...)` and `..._(...)^(...)`.
-   `u (...)`, `b (...) (...)` where `u` and `b` are unary and binary operators

If rendering them is desired, double the delimiter; for example: `((x+y))/2` or `{: (x+y) :}/2`

### Matrices

An expression of the form `(a & b ;; c & d ;; e & f)` is interpreted and rendered as a
matrix. More precisely:
-   Matrices are always rendered using LaTeX's (`amsmath`'s) `matrix` environment.
-   `&` delimits columns while `;;` delimits rows, like LaTeX's ` \\ `.
-   The outer delimiters will be rendered as usual with `\left` and `\right`.
-   Delimiters are *not* actually necessary; for instance, this is a valid matrix:

    ```
    x + 1 &=& y ;;
    x     &=& y - 1 ;;
          &=& 0.
    ```

    As shown, this is useful for long, aligned equations.  Eventually it may become possible to specify column alignments (see issue #19).

## License

The code of AsciiMath is released under the MIT license
