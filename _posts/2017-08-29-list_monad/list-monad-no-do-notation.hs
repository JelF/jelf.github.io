#!/usr/bin/env runhaskell

import Control.Monad (guard)

main :: IO ()
main = sequence_ $
  ["vasya", "petya", "world"] >>= \name ->
    (if name == "vasya" then ["messages", "posts"] else ["messages"]) >>= \entities ->
      ([1..5] :: [Integer]) >>= \count ->
        guard ((entities == "messages") || (count < 3)) >>
        return (
          putStrLn ("Hello, " ++ name) >>
          putStrLn (show count ++ " " ++ entities ++ " for you")
        )
