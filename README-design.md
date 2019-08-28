# Design of the hnix code base

Welcome to the hnix code! You may notice some strange things as you venture
into this realm, so this document is meant to prepare you, dear reader, for
the secrets of the labyrinth as we've designed them.

The first thing to note is that hnix was primarily designed so that Haskell
authors could use it to craft custom tooling around the Nix ecosystem. Thus,
it was never fully intended for just the end user. As a result, we use a great
deal of abstraction so that these enterprising Haskell authors may inject
their own behavior at various points within the type hierarchy, the value
representation, and the behavior of the evaluator.

To this end, you'll see a lot of type variables floating around, almost
everywhere. These provide many of the "injection points" mentioned above.
There is a strict convention followed for the naming of these variables, the
lexicon for which is stated here.

`t` is the type of thunks. It turns out that hnix dosen't actually need to
know how thunks are represented, at all. It only needs to know that the
interface can be honored: pending action that yield values may be turned into
thunks, and thunks can later be forced into values.

`f` is the type of a comonadic and applicative functor that gets injected at
every level of a value's recursive structure. In the standard evaluation
scheme, this is used to provide "provenance" information to track which
expression context a value originated from (e.g., this 10 came from that
expression "5 + 5" over in this file, here).

`m` is the "monad of evaluation", which must support at least the features
required by the evaluator. The fact that the user can evaluate in his own base
monad makes it possible to create custom builtins that make use of arbitrary
effects.

`v` is the type of values, which is almost always going to be `NValue t f m`,
though it could be `NValueNF t f m`, the type of normal form values. Very few
points in the code are generic over both.

## Different value types

Having said that, I should mention that there are two different types of
values: `NValue` and `NValueNF`. The former is created by evaluating an
`NExpr`, and then latter by calling `normalForm` on an `NValue`.

## Exception types

Exception type constructors start with `EA` or `ES`, exception data constructors start
with an `E`, where: `E` - *exception*, `A` - *asynchronous*, `S` - *synchronous*.
