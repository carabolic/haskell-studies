---
format: markdown+lhs
...

Sequence and Discard
================================================================================

Abstract
--------
I propose a novel series of `Sequence and Discard` operators which purpose is to
sequenc actions and discard their results. These operators are introduced just
for convenience and brevity. As shown in this document they also help to prevent
a specific class of errors caused by bad spelling.

Introduction
--------------------------------------------------------------------------------

The modules `Control.Applicative`{.haskell} and `Control.Monad`{.haskell}
already provide numerous convenience function concerning `Applicative Functors`
and `Monads`. For instance 

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .haskell }
    (*>) :: Applicative f => f a -> f b -> f b
    (<*) :: Applicative f => f a -> f b -> f a
    (>>) :: Monad m => m a -> m b -> m b
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allow to sequence actions (either applicative or monadic) from left to right
and discarding the first or second result and return the other.

Sometimes it is convenient to sequence actions and discard the results all
together.

In this document a `Sequence and Discard` operator is proposed that does exactly
that. It sequences two actions left to right or vice versa and completly
discards their results.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .haskell }
    (**>) :: Applicative f => f a -> f b -> f ()
    (<**) :: Applicative f => f a -> f b -> f ()
    (>>>>) :: Monad m => m a -> m b -> m ()
    (<<<<) :: Monad m => m a -> m b -> m ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This document is written using literate Haskell. The source code can be found at
[GitHub]().

For the motivating example I'm going to use
[Attoparsec](http://hackage.haskell.org/package/attoparsec) and in particular
the `Text`{.haskell} parser. For brevity `OverloadedStrings` language pragma is
used, such that ordinary strings `[Char]`{.haskell} are automatically converted
to `Data.Text`{.haskell}.

> {-# LANGUAGE OverloadedStrings #-}
> module Combinators where
> 
> import Control.Applicative (Applicative, pure, (*>), (<*))
> import Data.Attoparsec.Text
> import Data.Text

The remainder of this document is structured as follows. First a motivating
example is given, second the proper definitions for the *Sequence and Discard*
operators a given and used in the last section in order to showcase their
usefulness.

Motivation
--------------------------------------------------------------------------------

The string to be parsed

> input :: Text
> input = "number:10; symbol:a;\n"


The abstract data type to be parsed in

> data MyData = MyData { number :: Int , symbol :: Char } deriving (Show)


Error prone parsing function

> errorProneParse :: Parser MyData
> errorProneParse = do
>     n <- string "number:" *> decimal <* char ';' <* skipSpace
>     -- the next line is prone to errors
>     s <- string "symbol:" *> anyChar <* char ';' <* endOfLine
>     --                               ^^ one might introduce errors here
>     return $ MyData n s

Definitions
--------------------------------------------------------------------------------

Applicative sequence (from left to right) and discard

> (**>) :: Applicative f => f a -> f b -> f ()
> f **> g = f *> g *> pure ()


Applicative sequence (from right to left) and discard

> (<**) :: Applicative f => f a -> f b -> f ()
> (<**) = flip (**>)


Sequence actions from left to right, discarding both results

> (>>>>) :: Monad m => m a -> m b -> m ()
> f >>>> g = f >> g >> return ()


Sequence actions from right to left, discarding both results

> (<<<<) :: Monad m => m a -> m b -> m ()
> (<<<<) = flip (>>>>)


Examples
--------------------------------------------------------------------------------

Error prone parsing function
This time with type that type checks but leads to wrong results

> errorProneParseTypo :: Parser MyData
> errorProneParseTypo = do
>     n <- string "number:" *> decimal <* char ';' <* skipSpace
>     s <- string "symbol:" *> anyChar *> char ';' <* endOfLine
>     --                               ^^ this should be <*
>     return $ MyData n s


Better parser routine using applicative *Sequence and Discard* operator.

> betterParseA :: Parser MyData
> betterParseA = do
>     n <- string "number:" *> decimal <* char ';' **> skipSpace
>     s <- string "symbol:" *> anyChar <* char ';' **> endOfLine
>     return $ MyData n s

This won't type check, due to incorrect types `MyData Int ()`{.haskell} instead
of `MyData Int Char`{.haskell}.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .haskell }
    betterParseATypo :: Parser MyData
    betterParseATypo = do
        n <- string "number:" *> decimal <* char ';' **> skipSpace
        s <- string "symbol:" *> anyChar *> char ';' **> endOfLine
        return $ MyData n s
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


> betterParseM :: Parser MyData
> betterParseM = do
>     n <- string "number:" *> decimal <* char ';' >>>> skipSpace
>     s <- string "symbol:" *> anyChar <* char ';' >>>> endOfLine
>     return $ MyData n s


Same typo as in errorProneParseTypo This time it doesn't type check
Expected MyData Int Char
Actual   MyData Int ()

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ { .haskell }
    betterParseMTypo :: Parser MyData
    betterParseMTypo = do
        n <- string "number:" *> decimal <* char ';' >>>> skipSpace
        s <- string "symbol:" *> anyChar *> char ';' >>>> endOfLine
        return $ MyData n s
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

