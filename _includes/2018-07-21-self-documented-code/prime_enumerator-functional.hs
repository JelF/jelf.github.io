module Data.Primes (
  primes
) where

import Data.List
import qualified Data.List.Ordered as Ordered

data EratosthenesSieveSlice = EratosthenesSieveSlice {
  entities :: [Integer]
, lower :: Integer
, upper :: Integer
}

primes :: [Integer]
primes = recursive [] 1
  where
    recursive primes upper =
      let slice = makeEratosthenesSieveSlice (upper + 1) (upper * 2)
          -- all numbers in slice could not be multiplier of other number
          -- so we can remove only known primes
          newPrimes = entities $
            foldl (removePrimeMultipliersFromSlice) slice primes
      -- as it is infinite recursion we should yield new primes imediatly
      in newPrimes ++ recursive (primes ++ newPrimes) (upper * 2)

makeEratosthenesSieveSlice :: Integer -> Integer -> EratosthenesSieveSlice
makeEratosthenesSieveSlice lower upper = EratosthenesSieveSlice {
  entities = [lower..upper]
, lower = lower
, upper = upper
}

removePrimeMultipliersFromSlice ::
  EratosthenesSieveSlice -> Integer -> EratosthenesSieveSlice

removePrimeMultipliersFromSlice slice prime =
  let multipliers = iterate (+ prime) prime
  in EratosthenesSieveSlice{
    entities = entities slice `Ordered.minus` multipliers
  , lower = lower slice
  , upper = upper slice
  }
