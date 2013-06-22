# sexp-rewrite

sexp-rewrite (sexprw for short) is an Emacs package for doing
pattern-based rewriting of sexp-structured code---ie, code in Lisp,
Scheme, and primarily Racket.

Some examples of "pattern-based rewriting" are:

 - Turn a chain of `if` expressions into a `cond` expression.

 - Rewrite an application of `map` with `lambda` to `for/list`---or `andmap` to `for/and`, etc.

 - Turn a `letrec` into a sequence of internal definitions.

The pattern language is simple enough that you can define your own ad
hoc rewriting rules.

Transformations preserve comments and are halfway decent at preserving
formatting, with the occasional help of appropriate spacing
annotations.

## Try it out

Visit `sexp-rewrite.el` and evaluate the buffer (`M-x eval-buffer`).
Then visit `racket-rewrites.el` and evaluate that buffer too.

Most rewriting rules (called "tactics") have examples after them.
Go to the example labeled "`example for if-to-cond...`" and place the cursor at the first left parenthesis---that is, at `(if (< x 10)...`.

Run the `if-to-cond` tactic by entering `M-x sexprw-execute-tactic` and then `if-to-cond`. 
The `if` expression gets rewritten to a `cond` expression---but only the first `if`; there are still `if`s left in the `else` branch.
Now run the `cond-else-absorb-if` tactic. There's a keybinding for `sexprw-execute-tactic` that makes executing a specific tactic quicker: `C-c C-d x`. Then type `cond-else-absorb-if` at the prompt (you can use tab completion to save typing).
The `if` expression gets "absorbed" into the `cond`. There's just one `if` left, but it's inside a `let`, so let's leave it alone for now.

What a bother to have to type in the tactic names. Fortunately, there's an even quicker way.

Undo twice (`C-x u C-x u`) to reset the example to the original form, and make sure the cursor is back at the beginning of the example.
Now enter `C-c C-d e` (which runs the `sexprw-auto-expression` command). The command automatically tries a bunch of tactics until one works, and it reports the name of the tactic in the minibuffer.

Faster?

Undo once (`C-x u`) to reset the example again.
Now type `C-c C-d r e`, which repeatedly tries tactics (up to about a hundred times) until they stop working. This time we get the first two `if` expressions in one shot.

Type `C-c C-d x unsafe-cond-else-absorb-let-if` (tab completion is your friend!) to absorb the `if` under the `let`. It's not in the default tactic list because it's *unsafe*: it removes the else-branch expression from the scope of the `let`-bound variable. That's probably usually fine, because the variable is false in the else branch, so there's little point in referring to it.

## How to define tactics

To be continued.
