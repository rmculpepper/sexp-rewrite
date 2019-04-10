# sexp-rewrite Reference

## Defining Tactics and Nonterminals

A tactic consists of a pattern and a template:

    (define-sexprw-tactic NAME PATTERN TEMPLATE)

A nonterminal consists of one or more patterns:

    (define-sexprw-nt NAME
      MAYBE-ATTRS
      (pattern PATTERN WITH-CLAUSE ...) ...)

    where MAYBE-ATTRS = ε | :attributes (ATTR-NAME ...)
          WITH-CLAUSE = :with PATTERN TEMPLATE

Pattern variables defined inside of a nonterminals patterns are available as attributes of instances of the nonterminal using the syntax `$pvar.$attr` (see below).

A tactic name can also be used as a nonterminal name. In addition to the tactic's pattern variables, it also exports an attribute named `$out` with the result of the template.


## Patterns

A pattern is one of the following:

- `symbol` : matches that literal symbol. The symbol cannot start with a `$` character.
- `$name` : matches any sexp and binds it to the pattern variable `$name`.
- `$name:nt` : matches an occurrence of the nonterminal `nt` and binds it to the pattern
  variable `$name`.
- `(pattern1 ... patternN)` : ie, a list of patterns (the `...` are not literal)
  matches a parenthesized sequence of N terms where each term matches the corresponding
  pattern.
- `(!@ pattern1 ... patternN)` : matches a non-parenthesized sequence of N terms where
  each term matches the corresponding pattern.
- `(!SPLICE pattern1 ... patternN)` : equivalent to `(!@ pattern1 ... patternN)`.
- `pattern ...` : (ie, a pattern followed by literal ellipses) matches zero or more occurrences
  of the pattern
- `(!OR pattern1 ... patternN)` : matches if any of the given patterns match
- `(!AND pattern1 ... patternN)` : matches if all of the given patterns match
- `(!GUARD pattern expr)` : to be documented...

## Templates

- `symbol` : produces that literal symbol
- `$name` : produces the text bound to the pattern variable `$name`
- `$name.$attr` : produces the text bound to the attribute `$attr` of the pattern variable
  `$name`, which must be bound to a nonterminal the defines `$attr`.
- `(template1 ... templateN)` : produces a parenthesized sequence consisting of the results
  of the N templates.
- `(!@ template1 ... templateN)` : produces a non-parenthesized sequence consisting of the
  results of the N templates.
- `(!SPLICE template1 ... templateN)` : equivalent to `(!@ template1 ... templateN)`
- `[template1 ... templateN]` : produces a square-bracketed sequence consisting of the
  results of the N templates.
- `(!SQ template1 ... templateN)` : equivalent to `[template1 ... templateN]`.
- `!NL` : prefers a new line before the next non-empty template.
- `!SP` : prefers a space before the next non-empty template.
- `!SL` : prefers a new line before the next template, if its contents span multiple lines.
- `!NOSP` : prevents any space before the next template
- `template ...` : (ie, a template followed by literal ellipses) produces zero or more
  instantiations of the template, based on its pattern variables.

## Built-in Nonterminals

The following nonterminals are defined by the sexp-rewrite library:

- `pure-sexp` : matches a single sexp
- `sexp` : matches a single sexp, which may have comments preceding it
- `id` : matches any atom (note: currently includes numbers, etc, too)
- `rest` : matches the rest of the enclosing sexp, including comments
  and terms; useful for function bodies, for example


## Examples

This section illustrates the sexprw tactic language with some examples
from the `sexprw-for-racket.el` file.

The `define-absorb-lambda` tactic turns a definition of a variable
with an explicit `lambda` expression into a function-style definition.

    (define-sexprw-tactic define-absorb-lambda
      (define $name:id (lambda ($arg ...) $body:rest))
      (define ($name $arg ...) !NL $body))

The following tactic rewrites a `let` binding followed by a test of
the `let`-bound variable into `cond` with the `=>` notation. Note the
two occurrences of `$name` in the pattern; the pattern only matches if
the same identifier is used in both places. Note also that this tactic
is unsafe if the `let`-bound variable occurs free in the else branch
of the `if` expression---be careful when using tactics.

    (define-sexprw-tactic let-if-to-cond
      ;; Unsafe if $name occurs free in $else
      (let ([$name:id $rhs])
        (if $name:id $then $else))
      (cond [$rhs !SL => (lambda ($name) !SL $then)] !NL
            [else !SL $else]))

The following nonterminal definition gives the form of a `let`
binding, including a special case for function right-hand sides. It
also defines the attribute `$def` as the natural corresponding
definition syntax.

    (define-sexprw-nt let-clause
      :attributes ($def)
      (pattern [$name:id (lambda ($arg ...) $body:rest)]
               :with $def (define ($name $arg ...) !SL $body))
      (pattern [$name:id $rhs]
               :with $def (define $name !SL $rhs)))

The `letrec-to-definitions` tactic uses the `let-clause` nonterminal
and its `$def` attribute to turn a `letrec` expression's bindings into
internal definitions.

    (define-sexprw-tactic letrec-to-definitions
      (letrec ($c:let-clause ...) $body:rest)
      (let () !NL (!@ $c.$def !NL) ... $body))
