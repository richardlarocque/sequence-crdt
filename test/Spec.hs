{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Test.Hspec
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Data.List
import Data.Word
import Control.Monad

import SiteState
import Dense

import Logoot as LO
import Upos as U
import Lseq as LS

import Debug.Trace

main :: IO ()
main = do hspec spec

shouldBeBetween b (x,y) =
  assertBool (intercalate " < " $ map show [x, b, y]) (x < b && b < y)

-- TODO: Remove unnecessary parameter
denseSpecs :: forall a . (Eq a, Dense a, Arbitrary a, Show a) => String -> Spec
denseSpecs name = do
  describe name $ do
    let mkMin = makeLeast :: a
    let mkMax = makeGreatest :: a
    let mkBetween = makeBetween :: SiteState -> a -> a -> a

    describe "instance Eq" $ do
      specify "least element is equal to itself" $ do
        mkMin == mkMin

      specify "greatest element is equal to itself" $ do
        mkMax == mkMax

      specify "least is not equal to greatest" $ do
        mkMax /= mkMin

      specify "identical positions are identical" $ do
        property $ \p -> (p :: a) == p

    describe "instance Ord" $ do
      specify "greatest position is greater than any other" $ do
        property $ \p -> (p :: a) <= mkMax

      specify "smallest position is smaller than any other" $ do
        property $ \p -> mkMin <= (p :: a)

      specify "no position is less than itself" $ do
        property $ \p -> not $ (p :: a) < p

    describe "instance Dense" $ do
      let mySite = SiteState (SiteId 10) (SiteClock 2)
      let insertsBetween s x y = do x `shouldSatisfy` (< y)
                                    let z = mkBetween s x y
                                    z `shouldBeBetween` (x,y)

      it "can insert between greatest and smallest" $ do
        insertsBetween mySite mkMin mkMax

      it "can insert between for arbitrary positions" $ do
        property $ \x y -> x < y ==> insertsBetween mySite x y

uposCompressSpecs = do
  describe "Upos compression" $ do
    it "is reversible" $ do
      property $ \x -> x /= U.MinPos && x /= U.MaxPos ==>
        x == (decompress . compress) x

    it "maintains sort ordering" $ do
      property $ \x y ->
        x /= U.MinPos && x /= U.MaxPos &&
        y /= U.MinPos && y /= U.MaxPos &&
        x < y ==> compress x < compress y

spec = do denseSpecs @LO.Position "Logoot"
          denseSpecs @U.Upos "Upos"
          denseSpecs @LS.Position "Lseq"

          uposCompressSpecs
