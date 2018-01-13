{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where

import SiteState
import Dense
import Lseq as LS
import Logoot as L
import Upos as U

import Data.Binary
import Data.Int
import Data.List

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import qualified Data.ByteString.Lazy as BL

encodedLength :: (Binary a) => a -> Int
encodedLength = fromIntegral . BL.length . encode

stressLeftInsertion :: forall a . (Dense a) => Int -> SiteState -> a -> a -> a
stressLeftInsertion 0 ss _ y = y
stressLeftInsertion n ss x y = stressLeftInsertion (n-1) ss x (makeBetween ss x y)

stressRightInsertion :: forall a . (Dense a) => Int -> SiteState -> a -> a -> a
stressRightInsertion 0 ss x _ = x
stressRightInsertion n ss x y = stressRightInsertion (n-1) ss (makeBetween ss x y) y

benchmark :: forall a . (Dense a, Arbitrary a, Show a, Binary a) => String -> (a -> Int)-> IO ()
benchmark name lengthFn = do
  (x,y) <- generate (arbitrary :: Gen (a, a))
  let [left, right] = sort [x, y]
  site <- generate (arbitrary :: Gen SiteState)

  putStrLn $ "Tests for " ++ name
  putStrLn $ "  stress left insertion:"
  let l10 = stressLeftInsertion 10 site left right
  putStrLn $ "     10 rounds: " ++ (show $ lengthFn l10)
  let l100 = stressLeftInsertion 100 site left right
  putStrLn $ "    100 rounds: " ++ (show $ lengthFn l100)
  let l1000 = stressLeftInsertion 1000 site left right
  putStrLn $ "   1000 rounds: " ++ (show $ lengthFn l1000)
  let l10000 = stressLeftInsertion 10000 site left right
  putStrLn $ "  10000 rounds: " ++ (show $ lengthFn l10000)

  putStrLn $ "  stress right insertion:"
  let l10 = stressRightInsertion 10 site left right
  putStrLn $ "     10 rounds: " ++ (show $ lengthFn l10)
  let l100 = stressRightInsertion 100 site left right
  putStrLn $ "    100 rounds: " ++ (show $ lengthFn l100)
  let l1000 = stressRightInsertion 1000 site left right
  putStrLn $ "   1000 rounds: " ++ (show $ lengthFn l1000)
  let l10000 = stressRightInsertion 10000 site left right
  putStrLn $ "  10000 rounds: " ++ (show $ lengthFn l10000)

main :: IO ()
main = do benchmark @L.Position "Logoot" encodedLength
          benchmark @LS.Position "Lseq" encodedLength
          benchmark @U.Upos "Upos" (length . encodePosition)
          benchmark @U.Upos "Compressed Upos" (length . compress)
