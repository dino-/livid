{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Livid.Episode where

import Data.Aeson.Types
import Data.Time ( ZonedTime, formatTime, utcToLocalZonedTime )
import GHC.Generics ( Generic )
import System.Directory ( getModificationTime )
import System.FilePath
import System.Locale ( defaultTimeLocale )


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
      , "date"     .= formatTime defaultTimeLocale "%c" d
      , "playpath" .= p
      ]


-- Construct an episode
mkEpisode :: FilePath -> FilePath -> FilePath -> IO Episode
mkEpisode root progDir epFile = do
   let playpath' = root </> progDir </> epFile
   localt <- getModificationTime playpath' >>= utcToLocalZonedTime
   return $ Episode epFile localt playpath'
