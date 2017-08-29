#!/usr/bin/env runhaskell

import Control.Monad (guard)

main :: IO ()
main = sequence_ $ do
  name <- ["vasya", "petya", "world"]
  entities <- if name == "vasya" then ["messages", "posts"] else ["messages"]
  count <- [1..5] :: [Integer]
  guard ((entities == "messages") || (count < 3))

  return $ do
    putStrLn $ "Hello, " ++ name
    putStrLn $ show count ++ " " ++ entities ++ " for you"
