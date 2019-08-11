# lambda-calculus

Lambda calculus is the [God's Programming language](https://podcst.app/episode?feed=https%3A%2F%2Fcorecursive.libsyn.com%2Ffeed&title=God%27s%20Programming%20Language), so I wrote an interpreter for it.
This interpreter is an extension to the original lambda caculus as described
[here](http://www.cse.unt.edu/~tarau/teaching/PL/docs/Lambda%20calculus.pdf).

## Pre-requisites

  - cabal

## Extensions

  - Number and Booleans can be encoded in pure lambda calculus but for
    convenience they have been made primitives.

  - General operators related to `Int` and `Bool` have been added as well.

Added Types:
  - Int, literals are constructors
  - Bool, `true` and `false` constructors

Added Operators
  - (+) :: Int -> Int -> Int
  - (-) :: Int -> Int -> Int
  - (*) :: Int -> Int -> Int
  - (/) :: Int -> Int -> Int
  - (and) :: Bool -> Bool -> Bool
  - (or) :: Bool -> Bool -> Bool
  - not :: Bool -> Bool
  - (>) :: a -> a -> Bool
  - (<) :: a -> a -> Bool
  - (=) :: a -> a -> Bool

### Things you can do

We can define most all of the extensions, that I added to the language. In fact
most modern functional languages compile down to some form of lambda calculus.

```
true := \x.\y.x -- true literal defined in pure lambda calculus
false := lambda x.lambda y. y

and := \p.\q.p q p
or := lambda p.lambda q.p p q

0 := \f.\x.x
1 := lambda f.lambda x.f x
2 := \f.\x. f ( f x )
```

## REPL

You can try out the examples above in the repl.
`cabal new-run`, will open the repl

```
λ (((\p.\q.p q p)\x.\y.x)\x.\y.y)
\y.y
λ (((\p.\q.p p q)\x.\y.x)\x.\y.y)
\y.x
```
