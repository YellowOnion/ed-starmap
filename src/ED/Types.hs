{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

module ED.Types (
       System(..)
       ) where

import GHC.Generics
import Data.Aeson
import Data.Text
import Linear
import Control.DeepSeq
import Control.DeepSeq.Generics (genericRnf)

data System = System {
      name :: !Text
    , location :: !(V3 Double)
    , id_ :: !Int
    --, date :: !Text -- find more restrictive type
    } deriving (Generic, Show)

instance NFData System where rnf = genericRnf

type Galaxy = [System]

instance FromJSON System where
  parseJSON = withObject "system" $ \o -> do
    name <-   o .: "name"
    coords <- o .: "coords"
    x <- coords .: "x"
    y <- coords .: "y"
    z <- coords .: "z"
    id_ <- read <$> o .: "id"
    date :: Text <- o .: "date"
    return $ System name (V3 x y z) id_ --date
