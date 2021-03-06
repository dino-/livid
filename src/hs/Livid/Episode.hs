-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Livid.Episode where

import Data.Aeson.Types
import Data.Time ( ZonedTime, formatTime, utcToLocalZonedTime,
   zonedTimeToUTC )
import Data.Time.Format ( defaultTimeLocale )
import GHC.Generics ( Generic )
import System.Directory ( getModificationTime )
import System.FilePath


data Episode = Episode
   { title :: String
   , date :: ZonedTime
   , playpath :: FilePath
   }
   deriving (Show, Generic)

instance FromJSON Episode

instance ToJSON Episode where
   toJSON (Episode t d p) = object
      [ "title"    .= t
      , "date"     .= formatTime defaultTimeLocale
            "%A, %B %e, %Y at %l:%M:%S %p" d
      , "playpath" .= p
      ]

instance Eq Episode where
   x == y = ux == uy
      where
         ux = zonedTimeToUTC . date $ x
         uy = zonedTimeToUTC . date $ y

instance Ord Episode where
   x <= y = ux <= uy
      where
         ux = zonedTimeToUTC . date $ x
         uy = zonedTimeToUTC . date $ y


-- Construct an episode
mkEpisode :: FilePath -> FilePath -> FilePath -> IO Episode
mkEpisode root progDir epFile = do
   let playpath' = root </> progDir </> epFile
   localt <- getModificationTime playpath'
      >>= utcToLocalZonedTime
   return $ Episode epFile localt playpath'
