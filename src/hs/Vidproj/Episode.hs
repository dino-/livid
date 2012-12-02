{-# LANGUAGE DeriveGeneric #-}

module Vidproj.Episode where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )


data Episode = Episode
   { title :: String
   , date :: String
   }
   deriving (Show, Generic)

instance FromJSON Episode
instance ToJSON Episode
