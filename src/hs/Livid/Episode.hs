{-# LANGUAGE DeriveGeneric #-}

module Livid.Episode where

import Data.Aeson ( FromJSON, ToJSON )
import Data.Time ( formatTime, utcToLocalZonedTime )
import GHC.Generics ( Generic )
import System.Directory ( getModificationTime )
import System.FilePath
import System.Locale ( defaultTimeLocale )


data Episode = Episode
   { title :: String
   , date :: String
   , playpath :: FilePath
   }
   deriving (Show, Generic)

instance FromJSON Episode
instance ToJSON Episode


{- Construct an episode with all info except date
-}
mkEpisode :: FilePath -> FilePath -> FilePath -> Episode
mkEpisode root progDir epFile = Episode epFile ""
   (root </> progDir </> epFile)


fillDates :: [Episode] -> IO [Episode]
fillDates = mapM $ \e -> do
   localt <- (getModificationTime $ playpath e) >>= utcToLocalZonedTime
   --let d = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" localt
   let d = formatTime defaultTimeLocale "%c" localt
   return $ e { date = d }
