---
layout: post
title: "Haskell Basics: How to Loop"
---
One of the things that really gets newcomers to Haskell is that it’s got a vision of flow control that’s completely foreign.  OCaml is arguably Haskell’s nearest popular cousin, and even it has basic things like while and for loops.

Throw in all this business with [endofunctors](http://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem) and [burritos](https://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/) and it’s pretty clear that a lot of newcomers get frustrated because all this theoretical stuff gets in the way of writing algorithms that they already know how to write.  In other languages, these newcomers are experts and they are not at all used to feeling lost.

As a preface, I’m not going to explain how monads work, and I’m not going to explain any of the historical anecdotes that explain why these things are the way they are.  This territory is incredibly well-trod by others.

Additionally, many of the things that I’ll describe here are non-idiomatic Haskell, but none create design-wrecking maintenance or performance problems.  I think it's better that newcomers write "ugly" code that works than it is that they learn all of functional programming all at once. `:)`

# Pure Loops

If your loop doesn’t require side effects, the thing you’re actually after is some kind of transform.  You want to turn a sequence into something else by walking it.

## Transforming Elements

If you just want to transform each element of a collection, but you don’t want to change the type of collection at all, you probably want a map.  The map function is called `map` and has this signature:

```haskell
map :: (a -> b) -> [a] -> [b]
```

If you don't have a list, but instead have a Vector, Map, deque or whatever, you can use its more general cousin `fmap`:

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

## Accumulating (aka folding)

Consider this simple JS:

```js
function count(anArray) {
    var result = 0;
    for (var i = 0; i < anArray.length; ++i) {
        result += anArray[i];
    }
    return result;
}
```

This clearly isn’t a map.  The result isn’t an array at all.  It’s something else.

When you want to walk an array and build up a value like this, use a fold.  The Haskell function you should start with is called `foldl'`, found in the `Data.Foldable` package.  The above transliterates to this Haskell:

```haskell
count l =
    let accumulate acc el = el + acc
    in foldl' accumulate 0 l
```

`foldl'` takes a function, an initial value and the collection to walk.  This function takes the result that has been computed so far, and the next element to merge in.

## Accumulations that exit early sometimes

Consider this:

```js
function indexOf(list, element) {
    for (var i = 0; i < list.length; ++i) {
        if (list[i] == element) {
            return i;
        }
    }
}
```

This is superficially similar to what we were doing above, but we want to stop looping when we hit a certain point.

Sadly, the best I can come up with requires composing a few things together:

```haskell
indexOf' list element =
    let test acc e
            | element == e = Left acc
            | otherwise    = Right (acc + 1)
    in case foldM test 0 list of
        Left i -> Just i
        Right _ -> Nothing
```

`foldM` comes from the `Data.Foldable` module.

In this example, `test` is a function that returns an `Either`.  If the function returns `Left` something, the loop will end and that will be the ultimate result of the `foldM` call.  If the function returns `Right` something, the loop will run one more iteration, and the returned value will become the new accumulator.

Note that this means it’s trivial to sense whether the loop actually ran to completion or if it stopped partway through.

# Impure Loops

## Just Plain Doing Stuff

`Data.Traversable` exports a function called `forM_` which takes a traversable data structure and a monadic function and it runs the action on each element, discarding the results.

This is as close to a C++-style `for()` loop as you’re going to get.

```haskell
main = do
    forM_ [1..100] $ \number -> do
        putStr $ show number ++ " "
        when (0 == number `mod` 3) $
            putStr "Fizz"
        when (0 == number `mod` 5) $
            putStr "Buzz"
        putStrLn ""
```

## Mapping
If you drop the underscore and use `forM` instead, you can capture the results.

```haskell
main = do
    strings <- forM [1..5] $ \number -> do
        putStr $ "Enter string " ++ show number ++ ": "
        getLine

    print strings
```

## Accumulating

Honestly, if it’s impure, you can just create an `IORef`.  `IORef`s are mutable variables in Haskell.

```haskell
main = do
    let increment n = n + 1

    count <- newIORef 0

    forM_ [0..50] $ \number -> do
        modifyIORef' count increment

    c <- readIORef count
    print c
```

## Better Accumulating

`foldM` is exactly analogous to `foldl'`, except it’s monadic.  This means that you can use it to perform side effects in your loop body as you accumulate values.

```haskell
main = do
    let l = [0..4]
    let iter acc element = do
            putStrLn $ "Executing side effect " ++ show element
            return (acc + element)
    total <- foldM iter 0 l
    putStrLn $ "Total is " ++ show total
```

## Accumulation with early termination

The simplest thing is to hand-roll a tail-recursive loop.

```haskell
main = do
    let test a_list = case a_list of
            [] ->
                return ()
            (x:xs) -> do
                putStrLn $ "Testing element " ++ show x
                if 0 == x `mod` 3
                    then return ()
                    else test xs
    test [1..10]
```

Here, our `test` function splices apart the list it is given, and stops if it is empty or if it divides evenly into 3.  If not, it tail recurses with the rest of the list.

Something useful to observe here is that we are, in a certain sense, effecting a "mutable variable" by way of the recursive call.  The parameter "shrinks" with each successive recursive step.

This is also the most flexible way to write a loop.  Anything you can do in C, you can do in Haskell by way of variations on this template.
