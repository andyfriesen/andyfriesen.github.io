---
layout: post
title: "Testable IO in Haskell"
---

At IMVU, we write a lot of tests.  Ideally, we write tests for every feature and bugfix
we write.  The problem we run into is one of scale: if each of IMVU's tests were 99.9% reliable, we would never
see a successful test run.

Tests erroneously fail for lots of reasons: the test could be running in the midst of the "extra" daylight-savings hour
or a leap day (or a leap second!).  The database could have been left corrupted by another test.  CPU scheduling could prioritize one
process over another.  Maybe the random number generator just so happened to produce two zeroes in a row.

All of these things boil down to the same root cause: nondeterminism within the test.

We've done a lot of work at IMVU to isolate and control nondeterminism in our test frameworks.  One of my favourite
techniques is the way we make our Haskell tests provably perfectly deterministic.

Here's how it works.

This post is Literate Haskell, which basically means you can point GHC at it directly and run it.
You can download it [here](https://raw.githubusercontent.com/andyfriesen/andyfriesen.github.io/master/_lhs/testable-io-in-haskell.lhs).

We'll start with some boilerplate.

```haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.State.Lazy as S
```

What we're looking to achieve here is a syntax-lightweight way of writing side effectful logic in a way that permits
easy unit testing.

In particular, a property we'd very much like to have is the ability to deny our actions access to IO when they are
running in a unit test.

For this example, we'll posit that the very important business action we wish to test is to prompt the user for their
name, then say hello:

```haskell
importantBusinessAction = do
    writeLine "Please enter your name: "
    name <- readLine
    if "" == name
        then do
            writeLine "I really really need a name!"
            importantBusinessAction
        else
            writeLine $ "Hello, " ++ name ++ "!"
```

We'll achieve this by defining a class of monad in which testable side effects can occur.  We'll name this class
`World`.

```haskell
class Monad m => World m where
    writeLine :: String -> m ()
    readLine :: m String
```

We can now write the type of our `importantBusinessAction`:

```haskell
importantBusinessAction :: World m => m ()
```

The name of this type can be read as "an action producing unit for some monad `m` in `World`."

When our application is running in production, we don't require anything except IO to run, so it's perfectly sensible
for `IO` to be a context in which `World` actions can be run.  The Haskell Prelude already offers the exact functions
we need, so this instance is completely trivial:

```haskell
instance World IO where
    writeLine = putStrLn
    readLine = getLine
```

In unit tests, we specifically want to deny access to any kind of nondeterminism, so we'll use the `State` monad.
`State` provides the illusion of a mutable piece of data through a pure computation.  We'll pack the state of our
application up in a record.

```haskell
type FakeIO = S.State FakeState
```

Aside from reliability, this design has another very useful property: It is impossible for tests to interfere with one
another even if many tests share the same state.  This means that "test fixtures" can trivially be effected by simply
running an action and using the resulting state in as many tests as desired.

The state record `FakeState` itself essentially captures the full state of the fake application at any one moment.

The `writeLine` implementation is very easy: We just need to accumulate a list of lines
that were printed.  We can carry that directly in our state record.

The `readLine` action is a bit more complicated.  We're going to write all kinds of tests for our application, and we
really don't want to burn any one particular behaviour into the framework.  We want to parameterize this on a per-test
basis.

We'll solve this by embedding an action directly into our state record.

```haskell
data FakeState = FS
    { fsWrittenLines :: [String]
    , fsReadLine     :: FakeIO String
    }

def :: FakeState
def = FS
    { fsWrittenLines = []
    , fsReadLine = return ""
    }
```

Now, given this record, we can declare that `FakeIO` is also a valid `World` `Monad`, and provide
implementations for our platform when run under unit test.

```haskell
instance World (S.State FakeState) where
    writeLine s = do
        st <- S.get
        let oldLines = fsWrittenLines st
        S.put st { fsWrittenLines = s:oldLines }

    readLine = do
        st <- S.get
        let readLineAction = fsReadLine st
        readLineAction
```

We also write a small helper function to make unit tests read a bit more naturally:

```haskell
runFakeWorld :: b -> State b a -> (a, b)
runFakeWorld = flip S.runState
```

Now, let's write our first unit test.

```haskell
main :: IO ()
main = do
```

We wish to test that our application rejects the empty string as a name.  When the user does this, we wish to verify
that the customer sees an error message and is asked again for their name.

First, we'll craft a `readLine` implementation that produces the empty string once, then the string "Joe."

Making this function more natural without compromising extensibility is left as an exercise to the reader. :)

Note that by providing the type `FakeIO String`, we have effectively authored an action that can _only_ be used in a
unit test.  The compile will fail if testable code calls this, but not vice versa.

```haskell
    let readLine_that_is_incorrect_once :: FakeIO String
        readLine_that_is_incorrect_once = do
            S.modify (\s -> s { fsReadLine = return "Joe" })
            return ""
```

Now that we have that, we can create a `FakeState` that represents the scenario we wish to test.

```haskell
    let initState = def
            { fsReadLine = readLine_that_is_incorrect_once }
```

And go!

```haskell
    let ((), endState) = runFakeWorld initState importantBusinessAction
```

Note that `runFakeWorld` produces a pair of the result of the action and the final state.  We can inspect this record
freely:

```haskell
    forM_ (reverse $ fsWrittenLines endState) $ \line ->
        print line
```

That's it!

In a real application, your `FakeState` analogue will be much more complex, potentially including things like a clock,
a pseudo-random number generator, and potentially state for a pure database of some sort.  Some of these things are
themselves complex to build out, but, as long as those implementations are pure, everything snaps together neatly.

If complete isolation from IO is impractical, this technique could also be adjusted to run atop a `StateT` rather than
pure `State`.  This allows for imperfect side-effect isolation where necessary.

Happy testing!

[Source Code](https://raw.githubusercontent.com/andyfriesen/andyfriesen.github.io/master/_lhs/testable-io-in-haskell.lhs).
