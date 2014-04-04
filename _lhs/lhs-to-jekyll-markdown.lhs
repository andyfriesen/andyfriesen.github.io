---
layout: post
title: Literate Haskell and Jekyll
---
I think I've more or less decided to switch my blog over to use [Github Pages](http://pages.github.com/) and
[Jekyll](http://jekyllrb.com/).  It's pretty neat and it means I can sleep safer knowing that I'm not inadvertently
inflicting unpatched PHP on some poor unsuspecting web host.

One of the things that's kind of annoying, though, is that Jekyll won't correctly handle Literate Haskell out of the
box.  You can drop code into a Markdown document and it will even syntax highlight it, but it doesn't support any
syntax that also happens to line up with Literate Haskell.

Clearly, this is a problem in need of a [self-referential solution](https://github.com/andyfriesen/andyfriesen.github.io/blob/master/_lhs/lhs-to-jekyll-markdown.lhs).

> {-# LANGUAGE OverloadedStrings #-}
> module Main where
>
> import System.IO (stdin, stdout, hIsEOF)
> import Control.Monad (when, unless)
> import qualified Data.Text as T
> import Data.Text.IO (hGetLine, hPutStrLn)

We run a very basic state machine: each line is either part of a Literate Haskell block, or it isn't.

> data State = LHS | Prose
>     deriving (Eq)
>
> processNextLine :: State -> IO ()
> processNextLine state = do
>     eof <- hIsEOF stdin
>
>     -- UPDATE 3 April 2014: My program had a bug! It would not close the last
>     -- Markdown group if the last line of the input program was Haskell code and
>     -- not prose.
>     when (eof && state == LHS) $
>         hPutStrLn stdout "```"
>
>     unless eof $ case state of
>         LHS   -> processLhsLine
>         Prose -> processProseLine

When looking at prose, all we need to do is watch out for lines that have bird tracks.  If it's anything else,
spit the line out verbatim.

> processProseLine :: IO ()
> processProseLine = do
>     line <- hGetLine stdin
>     if hasBirdTrack line
>         then
>             switchToLhs line
>         else do
>             hPutStrLn stdout line
>             processNextLine Prose

LHS is pretty much identical, but we need to strip the bird track from each line as we read it.

> processLhsLine :: IO ()
> processLhsLine = do
>     line <- hGetLine stdin
>     if hasBirdTrack line
>         then do
>             hPutStrLn stdout (stripBirdTrack line)
>             processNextLine LHS
>         else
>             switchToProse line

To flip between states, print out a line that signifies Haskell or not-Haskell, and resume in the new state.

It's kind of funny that we call this sort of recursion "functional."  In this program, it looks an awful lot like
"goto." :)

> switchToLhs :: T.Text -> IO ()
> switchToLhs line = do
>     hPutStrLn stdout "```haskell"
>     hPutStrLn stdout (stripBirdTrack line)
>     processNextLine LHS
>
> switchToProse :: T.Text -> IO ()
> switchToProse line = do
>     hPutStrLn stdout "```"
>     hPutStrLn stdout line
>     processNextLine Prose
>
> hasBirdTrack :: T.Text -> Bool
> hasBirdTrack line = ">" == line || "> " == T.take 2 line
>
> stripBirdTrack :: T.Text -> T.Text
> stripBirdTrack line = T.drop 2 line
>
> main :: IO ()
> main = processNextLine Prose

And that's it!

[Source](https://github.com/andyfriesen/andyfriesen.github.io/blob/master/_lhs/lhs-to-jekyll-markdown.lhs)
