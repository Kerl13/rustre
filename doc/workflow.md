# Compiler passes
See also Fig. 6 of the article.

## Lexing, Parsing
* Input: some Minilustre code
* Output: untyped Minilustre AST, with some constructors already translated into others (`if then else`, `e1 -> e2`, `pre`, according to [LCTES08a])
* Testing functions?

## Typing
> Typing is almost standard
* Output: typed Minilustre AST
* Testing functions?

## Clock annotation/inference
/!\ Looks like the article mentions clock annotation checking (Fig. 2), but we'll need to do clock annotation inference

* Output: typed and clock-annotated AST
* Testing functions?

## Normalization
* Output: a typed, clock-annotated AST in normal form
  The article mentions a grammar for these terms, but:
  > Note that it would also be possible to introduce a new intermediate
  > language instead of the source-to-source transformation. This is
  > essentially a matter of taste, the main advantage of the present
  > formulation being to save the redefinition of auxiliary notions.
* Testing functions?

## Dependencies and scheduling
This phase checks that there is no dependency loop/causality error and then creates a scheduling. Using just a topological sort isn't the best option.
* Output: typed, clock-annotated AST
* Testing functions?

## Nil analysis

## Translation to Obc

## Translation to Rust

### Possible optimizations
> The data-flow nature of this language makes the implementation of
> classical graph-based optimizations (e.g., copy elimination,
> common-subexpression elimination) particularly easy. We do not
> detail them here.

> the scheduling can contain heuristics that try to schedule
> consecutively equations that are guarded by the same clock. Then,
> the merging of consecutive control structures will be able to
> factorize more control conditions. Another classical optimization is
> related to the reuse of variables (which corresponds to removing
> copy variables in classical compilation terminology [19]). As
> mentioned in [15], a stream x and its previous value pre x can be
> stored in the same variable if the computation of x is not followed
> by a use of pre x.


## Translation into a simple object-based language
* Output: the AST of this object-based language
* Testing functions?
### Possible optimizations
> We only define the minimal intermediate language which is sufficient
> for the translation. One may consider a more general form with
> several methods, in particular to give access to the components of a
> structured output and avoid copying the output when calling a
> node. Nonetheless, this optimization is orthogonal to the
> translation and can be done afterwards.

## Translation into a target language
    Possible targets: Rust, Why3
* Testing functions?

# Extensions
## Optimizations
> We have also implemented three classical optimizations directly on
> the clocked-data flow language: inlining, dead-code removal, and
> automata minimization – the general form of common-subexpression
> elimination.
