{-# LANGUAGE DeriveGeneric #-}

module Livid.Episode where

import Data.Aeson ( FromJSON, ToJSON )
import GHC.Generics ( Generic )
import System.FilePath


data Episode = Episode
   { title :: String
   , date :: String
   , playpath :: FilePath
   }
   deriving (Show, Generic)

instance FromJSON Episode
instance ToJSON Episode


mkEpisode :: FilePath -> FilePath -> (FilePath, String) -> Episode
mkEpisode root progDir (epPath, date') = Episode epPath date'
   (root </> progDir </> epPath)
