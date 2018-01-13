module Dense where

-- TODO: Remove dependency on SiteState
import SiteState

class (Eq a, Ord a) => (Dense a) where
  makeLeast    :: a
  makeGreatest :: a
  makeBetween  :: SiteState -> a -> a -> a
