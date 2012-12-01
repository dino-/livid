{-# LANGUAGE DeriveGeneric #-}

module Vidproj.Directory where

import Control.Monad.Error
import Data.Function ( on )
import Data.Aeson ( FromJSON, ToJSON, encode )
import Data.ByteString.Lazy.Char8 hiding ( filter, notElem, map )
import Data.List
import GHC.Generics ( Generic )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.FilePath
import Text.Printf


data Episode = Episode
   { epTitle :: String
   , epDate :: String
   }
   deriving (Show, Generic)

instance FromJSON Episode
instance ToJSON Episode


data Program = Program
   { pTitle :: String
   , pEpisodes :: [Episode]
   }
   deriving (Show, Generic)

instance FromJSON Program
instance ToJSON Program


newtype Programs = Programs [Program]
   deriving (Show, Generic)

instance FromJSON Programs
instance ToJSON Programs


{- Get the contents of a directory and strip out the special . and
   .. dirs from the list
-}
contentsWithoutDots :: FilePath -> IO [FilePath]
contentsWithoutDots path =
   fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents path


{- Get all programs and their episodes given a root directory. Data
   is contained in a list of Program data structures
-}
getPrograms :: FilePath -> IO (Either String Programs)
getPrograms root = runErrorT $ do
   topExists <- liftIO $ doesDirectoryExist root
   unless topExists $ throwError $
      printf "Directory %s does not exist!" root

   progDirs <- liftIO $ contentsWithoutDots root
   programs <- liftIO $ mapM (getProgram root) progDirs

   return $ Programs $ sortBy (compare `on` pTitle) programs


{- Get an individual program and its episodes. Data is contained
   in a Program data structure
-}
getProgram :: FilePath -> FilePath -> IO Program
getProgram root dir = do
   epPaths <- liftIO $ contentsWithoutDots $ root </> dir
   let episodes = map (\title -> Episode title "") epPaths
   return $ Program dir (sortBy (compare `on` epTitle) episodes)
