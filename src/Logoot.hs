module Logoot(Position(..),Identifier(..)) where

-- TODO: Do not export Identifier

import Data.Word
import Data.List
import Data.Maybe
import Control.Monad

import Data.Binary

import SiteState

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import qualified Dense as D

data Identifier = Identifier Word32 SiteId
  deriving (Eq,Show)

data Position = MinPos | MaxPos | Position [Identifier]
  deriving (Eq,Show)

instance Ord Identifier where
  compare (Identifier id1 s1) (Identifier id2 s2) = compare (id1,s1) (id2,s2)

instance Ord Position where
  compare p q | p == q = EQ
  compare p q = compare (extendLow p) (extendLow q)

instance D.Dense Position where
  makeLeast    = Logoot.makeLeast
  makeGreatest = Logoot.makeGreatest
  makeBetween  = Logoot.makeBetween

instance Binary Identifier where
  get = error "not yet implemented"
  put (Identifier id (SiteId sid)) = do put id
                                        put sid

instance Binary Position where
  get = error "not yet implemented"
  put MaxPos = error "can not serialize MaxPos"
  put MinPos = error "can not serialize MinPos"
  put (Position ids) = putList ids

nilSite :: SiteId
nilSite = SiteId 0

makeLeast :: Position
makeLeast = MinPos

makeGreatest :: Position
makeGreatest = MaxPos

extendLow :: Position -> [Identifier]
extendLow MinPos = repeat (Identifier minBound nilSite)
extendLow MaxPos = repeat (Identifier maxBound nilSite)
extendLow (Position ids) = ids ++ repeat (Identifier minBound nilSite)

extendHigh :: Position -> [Identifier]
extendHigh MinPos = repeat (Identifier minBound nilSite)
extendHigh MaxPos = repeat (Identifier maxBound nilSite)
extendHigh (Position ids) = ids ++ repeat (Identifier maxBound nilSite)

makeBetween :: SiteState -> Position -> Position -> Position
makeBetween (SiteState sid _) p q =
  let p' = extendLow p
      q' = extendHigh q
      iter (x:xs) (y:ys) = case middleId sid x y of
        Nothing -> x:(iter xs ys)
        Just m  -> [m]
  in Position $ iter p' q'

middleId :: SiteId -> Identifier -> Identifier -> Maybe Identifier
middleId sid id1@(Identifier v1 s1) id2 =
  find (\c -> id1 < c && c < id2) candidates
  where candidates = [(Identifier v1 sid), (Identifier (v1+1) sid)]

idCount :: Position -> Int
idCount (Position ids) = Data.List.length ids

byteLength :: Position -> Int
byteLength p = (4 + 4) * idCount p

makeEvenSpacedList :: Int -> [Position]
makeEvenSpacedList n =
  let values = take n [0, 100 .. maxBound] in
  map (\v -> Position [Identifier v nilSite]) values

-- TODO: Move this somewhere sensible

instance Test.QuickCheck.Arbitrary Identifier where
  arbitrary = (liftM2 Identifier) (elements [0..3]) (arbitrary)

instance Test.QuickCheck.Arbitrary Position where
  arbitrary = Test.QuickCheck.frequency [
    (1, return makeLeast),
    (1, return makeGreatest),
    (10, (liftM Position) $ (scale (`div` 10) $ Test.QuickCheck.listOf1 arbitrary))
    ]
