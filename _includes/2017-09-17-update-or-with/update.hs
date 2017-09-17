#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns #-}

module Logger (
  Severity(Fatal, Error, Warn, Info, Debug)
, Logger(FileLogger)
, log
, runLogger

, main -- example
) where

import Prelude hiding (log)

import Control.Monad
import Control.Monad.Trans.Class

import qualified Control.Monad.Trans.State as State -- from mtl-2.2.1
import qualified System.IO as IO

data Severity = Fatal | Error | Warn | Info | Debug deriving (Eq, Ord)

data Logger = FileLogger {
  device :: IO.Handle
, severity :: Severity
, progname :: String
}

log :: Severity -> String -> State.StateT Logger IO ()
log severity' text = do
  FileLogger{severity, device} <- State.get
  when (severity >= severity') $
    lift $ IO.hPutStr device $ text ++ "\n"

runLogger :: Logger -> State.StateT Logger IO () -> IO ()
runLogger = flip State.evalStateT

--- example

main :: IO ()
main = runLogger logger $ do
  log Debug "debug disabled"
  State.state $ \ logger -> ((), logger{severity = Debug})
  log Debug "debug enabled"
  where
    logger = FileLogger { device=IO.stdout, severity=Info, progname="update"}
