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

import qualified Control.Monad.Trans.Reader as Reader -- from mtl-2.2.1
import qualified System.IO as IO

data Severity = Fatal | Error | Warn | Info | Debug deriving (Eq, Ord)

data Logger = FileLogger {
  device :: IO.Handle
, severity :: Severity
}

log :: Severity -> String -> Reader.ReaderT Logger IO ()
log severity' text = do
  FileLogger{severity, device} <- Reader.ask
  when (severity >= severity') $
    lift $ IO.hPutStr device $ text ++ "\n"

runLogger :: Logger -> Reader.ReaderT Logger IO () -> IO ()
runLogger = flip Reader.runReaderT

--- example

main :: IO ()
main = runLogger logger $ do
  log Debug "debug disabled"
  Reader.withReaderT (\logger -> logger{severity = Debug}) $
    log Debug "debug enabled"
  where
    logger = FileLogger { device=IO.stdout, severity=Info}
