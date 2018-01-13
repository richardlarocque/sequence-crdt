module Lseq(Position(..)) where

import Control.Monad
import Data.Maybe
import Data.Hashable
import Data.List
import Data.Word

import Data.Binary

import qualified Dense as D

import SiteState

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

initialBoundary :: Integer
initialBoundary = 8

data Identifier = Identifier Integer SiteId
  deriving (Eq,Show)

data Position = MinPos | MaxPos | Position [Identifier]
  deriving (Eq)

data Strategy = BoundaryPlus | BoundaryMinus
  deriving(Eq, Show)

instance Show Position where
  show MinPos = "MinPos"
  show MaxPos = "MaxPos"
  show (Position ids) =
    "[" ++ (intercalate ", " $ zipWith showBoundedId ids boundarySizes) ++ "]"
    where showBoundedId (Identifier value (SiteId sid)) boundary =
            "(" ++ (show value) ++ "/" ++ (show boundary) ++ ", " ++ (show sid) ++ ")"

instance Ord Position where
  compare p q | p == q = EQ
  compare p q = compare (extendLow p) (extendLow q)

instance Ord Identifier where
  compare (Identifier id1 s1) (Identifier id2 s2) = compare (id1,s1) (id2,s2)

instance D.Dense Position where
  makeLeast    = Lseq.makeLeast
  makeGreatest = Lseq.makeGreatest
  makeBetween  = Lseq.makeBetween

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
extendLow MinPos = repeat (Identifier 0 nilSite)
extendLow MaxPos = map (\x -> Identifier x nilSite) boundarySizes
extendLow (Position ids) = ids ++ repeat (Identifier 0 nilSite)

extendHigh :: Position -> [Identifier]
extendHigh MinPos = repeat (Identifier 0 nilSite)
extendHigh MaxPos = map (\x -> Identifier x nilSite) boundarySizes
extendHigh (Position ids) =
  let len = length ids
      highSuffixValues = drop len boundarySizes
      highSuffix = map (\x -> Identifier x nilSite) highSuffixValues
  in ids ++ highSuffix

makeBetween :: SiteState -> Position -> Position -> Position
makeBetween ss@(SiteState sid _) p q =
  let p' = extendLow p
      q' = extendHigh q
      plusSolution  = Position $ boundaryPlus  sid p' q'
      minusSolution = Position $ boundaryMinus sid (extendLow p) (extendHigh q)
  -- FIXME can plusSolution and minusSolution reside on different levels?
  in case strategyAtLevel ss (idsLength plusSolution - 1) of
    BoundaryPlus -> plusSolution
    BoundaryMinus -> minusSolution
  where idsLength (Position ids) = length ids

boundaryPlus :: SiteId -> [Identifier] -> [Identifier] -> [Identifier]
boundaryPlus sid (x:xs) (y:ys) =  case plusMiddle sid x y of
  Nothing -> x:(boundaryPlus sid xs ys)
  Just m  -> [m]

plusMiddle :: SiteId -> Identifier -> Identifier -> Maybe Identifier
plusMiddle sid id1@(Identifier v1 s1) id2 =
  find (\c -> id1 < c && c < id2) candidates
  where candidates = [(Identifier v1 sid), (Identifier (v1+1) sid)]

boundaryMinus :: SiteId -> [Identifier] -> [Identifier] -> [Identifier]
boundaryMinus sid (x:xs) (y:ys) =  case minusMiddle sid x y of
  Nothing -> x:(boundaryMinus sid xs ys)
  Just m  -> [m]

minusMiddle :: SiteId -> Identifier -> Identifier -> Maybe Identifier
minusMiddle sid id1@(Identifier v1 s1) id2@(Identifier v2 s2) =
  find (\c -> id1 < c && c < id2) candidates
  where candidates = [(Identifier (v2-1) sid), (Identifier v2 sid)]

strategyAtLevel :: SiteState -> Int -> Strategy
strategyAtLevel (SiteState (SiteId id) _) level =
  case even $ (hash (id + fromIntegral level)) of
    True  -> BoundaryPlus
    False -> BoundaryMinus

boundarySizes :: [Integer]
boundarySizes = iterate (*2) initialBoundary

-- TODO: Move this somewhere sensible

instance Test.QuickCheck.Arbitrary Position where
  arbitrary = Test.QuickCheck.frequency [
    (1, return makeLeast),
    (1, return makeGreatest),
    (10, scale (`div` 10) $ sized $ \n -> (liftM Position) $ sequence [ boundedIdentifier max | max <- take (n+1) boundarySizes])
    ]

boundedIdentifier :: Integer -> Gen Identifier
boundedIdentifier max =
  do num    <- choose (0, max - 1)
     siteId <- arbitrary
     return $ Identifier num siteId
