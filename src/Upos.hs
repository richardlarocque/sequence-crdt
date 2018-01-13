module Upos(Upos(..), decompress, compress, encodePosition, decodePosition) where

import Control.Monad
import Data.Word
import Data.Bits

import Data.Binary

import qualified Dense as D

import SiteState

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

data Upos = MinPos | MaxPos | Upos [Word8] Suffix

data Suffix = Suffix SiteId SiteClock

instance Show Upos where
  show MinPos = "MinPos"
  show MaxPos = "MaxPos"
  show x      = (show.encodePosition) x

instance D.Dense Upos where
  makeLeast    = makeLeast
  makeGreatest = makeGreatest
  makeBetween  = makeBetween

instance Binary Upos where
  put MaxPos = error "can not serialize MaxPos"
  put MinPos = error "can not serialize MinPos"
  put x = (putList . encodePosition) x
  get = error "not yet implemented"

makeSuffix :: SiteState -> Suffix
makeSuffix (SiteState id clk) = Suffix id clk

instance Ord Upos where
  compare p q | p == q = EQ
  compare p q = encodePosition p `compare` encodePosition q

instance Eq Upos where
  (==) MinPos MinPos = True
  (==) MaxPos MaxPos = True
  (==) p q = encodePosition p == encodePosition q

makeFromBytes :: SiteState -> [Word8] -> Upos
makeFromBytes ss pre = Upos pre (makeSuffix ss)

makeLeast :: Upos
makeLeast = MinPos

makeGreatest :: Upos
makeGreatest = MaxPos

makeBetween :: SiteState -> Upos -> Upos -> Upos
makeBetween ss p q =
  let p' = encodePosition p
      q' = encodePosition q
      suffix = makeSuffix ss
  in Upos (makeBetween' p' q') suffix

makeBetween' :: [Word8] -> [Word8] -> [Word8]
makeBetween' (x:xs) (y:ys) | x == y     = x:(makeBetween' xs ys)
makeBetween' (x:xs) (y:ys) | x + 1 == y = x:(makeGreater' xs)
makeBetween' (x:_)  (y:_)               = [avg x y]
makeBetween' [] [] = error "Unexpected equal positions in makeBetween"
makeBetween' [] ys                      = makeLesser' ys
makeBetween' xs [] = error "Unexpected left shorter than right"

avg x y = fromIntegral $ (((fromIntegral x) + (fromIntegral y)) :: Int) `div` 2

makeLesser' (0:xs) = 0:(makeLesser' xs)
makeLesser' (x:_)  = [x-1]

makeGreater' (255:xs) = 0:(makeGreater' xs)
makeGreater' (x:_)  = [x+1]
makeGreater' []     = [1]

------

-- Compression and encoding.

-- These intermediate data structures make the code clearer and debugging
-- simpler.  It's easier to express this algorithm in terms of
-- `UPos <-> [Block] <-> [Word8]` rather than `Upos <-> [Word8]`
data CountEncoding = High | Low
  deriving (Show)

data Block = RunLength Word8 Word32 CountEncoding
           | SimpleBlock [Word8]
           deriving (Show)

encodeSuffix :: Suffix -> [Word8]
encodeSuffix (Suffix (SiteId id) (SiteClock clk)) =
  encodeWord32 id ++ encodeWord32 clk

decodeSuffix :: [Word8] -> Suffix
decodeSuffix bs | length bs == 8 =
  let (siteBytes, clockBytes) = splitAt 4 bs
      site = decodeWord32 siteBytes
      clock = decodeWord32 clockBytes
  in Suffix (SiteId site) (SiteClock clock)
decodeSuffix _ = error "invalid length in decodeSuffix"

encodeWord32 :: Word32 -> [Word8]
encodeWord32 w =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]

decodeWord32 :: [Word8] -> Word32
decodeWord32 (b1:b2:b3:b4:[]) =
  (fromIntegral b1) `shiftL` 24 +
  (fromIntegral b2) `shiftL` 16 +
  (fromIntegral b3) `shiftL` 8 +
  fromIntegral b4
decodeWord32 _ = error "invalid length in decodeWord32"

-- Simple encoding to bytes without compression.
encodePosition :: Upos -> [Word8]
encodePosition (Upos prefix suffix) = (prefix) ++ encodeSuffix suffix
encodePosition MinPos = []
encodePosition MaxPos = (repeat maxBound)

decodePosition :: [Word8] -> Upos
decodePosition bytes =
  let len = length bytes
      (prefix, suffix) = splitAt (len - 8) bytes
  in Upos prefix (decodeSuffix suffix)

compress :: Upos -> [Word8]
compress = serializeBlocks . blockify . encodePosition

decompress :: [Word8] -> Upos
decompress = decodePosition . unblockify . deserializeBlocks

blockify :: [Word8] -> [Block]
blockify [] = []
blockify cs | canEncodeAsRunLength cs =
  let (b, rest) = makeRunLengthBlock cs in b:(blockify rest)
blockify cs =
  let (c8, rest) = splitAt 8 cs in [SimpleBlock c8] ++ blockify rest

canEncodeAsRunLength :: [Word8] -> Bool
canEncodeAsRunLength cs@(c1:_) = all (c1 == ) (take 4 cs) && (length (take 8 cs) == 8)
canEncodeAsRunLength _ = False

makeRunLengthBlock :: [Word8] -> (Block, [Word8])
makeRunLengthBlock cs@(c1:_) =
  let (rle, rest) = span (c1 ==) cs
      runLength = length rle
  {-
   - Encoding choice.  This is where the magic happens.
   -
   - We encode "high" if the repeated character we're abbreviating is smaller
   - than the character that follows the run.  In that case, a shorter run is
   - of higher value, and so the reversed count gets us the sort behavior we
   - want.
   -}
      encoding =
        if rest == [] then Low
        else if (head rest) > c1 then High
        else Low
  in (RunLength c1 (fromIntegral $ runLength) encoding, rest)

unblockify :: [Block] -> [Word8]
unblockify = concatMap unblockifyOne
  where unblockifyOne (RunLength c count _encoding) = replicate (fromIntegral count) c
        unblockifyOne (SimpleBlock cs) = cs

serializeBlocks :: [Block] -> [Word8]
serializeBlocks = concatMap serializeBlock

serializeBlock :: Block -> [Word8]
serializeBlock (SimpleBlock cs) = cs
serializeBlock (RunLength c count encoding) =
  (replicate 4 c) ++ (encodeCount encoding count)
  where encodeCount Low c = encodeWord32 c
        encodeCount High c = encodeWord32 (maxBound - c)

deserializeBlocks :: [Word8] -> [Block]
deserializeBlocks [] = []
deserializeBlocks cs | (length (take 8 cs) /= 8) = [SimpleBlock (take 8 cs)]
deserializeBlocks cs@(c1:_) | (all (c1 ==) (take 4 cs)) =
  let (blockBytes, rest) = splitAt 8 cs
      countBytes = drop 4 blockBytes
      (count, encoding) = decodeCount countBytes
  in (RunLength c1 count encoding):(deserializeBlocks rest)
deserializeBlocks cs =
  let (blockBytes, rest) = splitAt 8 cs
  in (SimpleBlock blockBytes):(deserializeBlocks rest)

decodeCount :: [Word8] -> (Word32, CountEncoding)
decodeCount bs | length bs == 4 = let value = decodeWord32 bs in
  if value > (maxBound `div` 2)
  then (maxBound - value, High)
  else (value, Low)
decodeCount _ = error "Invalid length in decodeCount"

------

-- TODO: Move this to some nicer place.
instance Test.QuickCheck.Arbitrary Upos where
  arbitrary = Test.QuickCheck.frequency [
    (1, return makeLeast),
    (1, return makeGreatest),
    (1, (liftM2 makeFromBytes) (arbitrary) (Test.QuickCheck.listOf1 arbitrary)),
    (10, (liftM2 makeFromBytes) (arbitrary) (arbitraryCompressible))
    ]

-- UniquePos values that trigger run-length encodings are much more interesting
-- from a test persepective.  Let's generate a few of those.
arbitraryCompressible :: Gen [Word8]
arbitraryCompressible = sized $ \n -> do
  pre <- listOf arbitrary
  repeatChar <- arbitrary
  post <- listOf arbitrary
  return $ pre ++ (replicate (12+n) repeatChar) ++ post

