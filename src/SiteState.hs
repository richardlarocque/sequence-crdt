module SiteState where

import Control.Monad
import Data.Word

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data SiteState = SiteState SiteId SiteClock

data SiteId = SiteId Word32
  deriving (Ord,Eq,Show)

data SiteClock = SiteClock Word32
  deriving (Ord,Eq,Show)

-- TODO: Move this somewhere sensible

instance Test.QuickCheck.Arbitrary SiteId where
  arbitrary = liftM SiteId $ (elements [1..10])

instance Test.QuickCheck.Arbitrary SiteClock where
  arbitrary = liftM SiteClock $ (elements [1..255])

instance Test.QuickCheck.Arbitrary SiteState where
  arbitrary = (liftM2 SiteState) arbitrary arbitrary
